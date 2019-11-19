{-# LANGUAGE OverloadedStrings #-}

module Dhaml.Internal
    ( DhamlException(..)
    , ParseState(..)
    , Stack
    , evaluateTag
    , initialState
    , run
    , withAnchor
    ) where

import Control.Exception (Exception)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.State (StateT, get, modify, runStateT)
import Data.Aeson.Types (JSONPath, JSONPathElement(..), formatPath)
import Data.ByteString.Char8 (ByteString)
import Data.Conduit (ConduitT, await, runConduit, yield)
import Data.Conduit.List (sourceList)
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Libyaml (Event(..))
import qualified Text.Libyaml as LY

data DhamlException
    = ExprError String
    | YamlError String

instance Show DhamlException where
    show (ExprError s) = s
    show (YamlError s) = s

instance Exception DhamlException

-- | Type synonym for the monad transformer stack
type Stack = StateT ParseState (ResourceT IO)

-- | State available to the monad transformer stack
data ParseState = ParseState
    { hasErrors :: Bool
    , path      :: JSONPath
    }

-- | The initial state
initialState :: ParseState
initialState = ParseState False []

-- | Run the conduit to encode a stream of 'Event's as a 'ByteString'
run :: ConduitT () Void Stack ByteString -> IO ByteString
run conduit = do
    result <- runResourceT $ runStateT (runConduit conduit) initialState
    if hasErrors $ snd result
        then exitFailure
        else return $ fst result

-- | A conduit for evaluating tagged 'EventScalar's using the specified handler
evaluateTag :: String -> a -> (a -> ByteString -> Stack [Event]) -> ConduitT Event Event Stack ()
evaluateTag tag ctx handler = next go
  where
    next f = do
        v <- await
        maybe (yamlError "Unexpected end of events") f v

    go event@EventStreamStart = do
        yield event
        next goStream
    go event = do
        yamlError $ "Unexpected event: " ++ show event

    goStream event@EventStreamEnd = do
        yield event
    goStream event@EventDocumentEnd = do
        yield event
        next goStream
    goStream event@EventDocumentStart = do
        yield event
        next goDocument
        next goStream
    goStream event = do
        yamlError $ "Unexpected event: " ++ show event

    goDocument event@EventDocumentEnd = do
        yield event
    goDocument event = do
        modifyPath $ const []
        goValue event

    goValue event@(EventMappingStart _ _ _) = do
        yield event
        next goMappingKey
    goValue event@(EventSequenceStart _ _ _) = do
        yield event
        next $ goSequence 0
    goValue event@(EventScalar expr t _ anchor) = do
        if t == (LY.UriTag tag) then do
            events <- lift . catch (handler ctx expr) $ \e -> do
                logError e
                return []
            sourceList $ withAnchor anchor events
        else
            yield event
    goValue event = do
        yield event

    goMappingKey event@EventMappingEnd = do
        yield event
    goMappingKey event@(EventScalar k _ _ _) = do
        modifyPath (++ [Key (decodeUtf8 k)])
        goValue event
        next goMappingValue
    goMappingKey event = do
        modifyPath (++ [Key "?"])
        goValue event
        next goMappingValue

    goMappingValue EventMappingEnd = do
        yamlError "Expected mapping value"
    goMappingValue event = do
        goValue event
        modifyPath init
        next goMappingKey

    goSequence _ event@EventSequenceEnd =
        yield event
    goSequence n event = do
        modifyPath (++ [Index n])
        goValue event
        modifyPath init
        next $ goSequence (n + 1)

-- | Update the path for the current YAML value
modifyPath :: (JSONPath -> JSONPath) -> ConduitT a b Stack ()
modifyPath f = lift . modify $ \x -> x { path = f (path x) }

-- | Format and print a YAML error
yamlError :: String -> ConduitT a b Stack ()
yamlError str = do
    state <- lift get
    lift . logError . YamlError $ format (formatPath $ path state) str
  where
    format p e = "\n" ++ red "Error" ++ " in " ++ cyan p ++ ": " ++ e ++ "\n"
    red s = "\ESC[1;31m" ++ s ++ "\ESC[0m"
    cyan s = "\ESC[1;36m" ++ s ++ "\ESC[0m"

-- | Print any errors as the YAML file is parsed and set the state accordingly
logError :: DhamlException -> Stack ()
logError e = do
    liftIO . hPutStrLn stderr $ show e
    modify $ \x -> x { hasErrors = True }

-- | Assign an 'Anchor' to the head of the 'Event' stream
withAnchor :: LY.Anchor -> [Event] -> [Event]
withAnchor _ [] = []
withAnchor anchor (event:rest) = replacement:rest
  where
    replacement = case event of
        LY.EventScalar v t s _      -> LY.EventScalar v t s anchor
        LY.EventSequenceStart t s _ -> LY.EventSequenceStart t s anchor
        LY.EventMappingStart t s _  -> LY.EventMappingStart t s anchor
        _ -> event
