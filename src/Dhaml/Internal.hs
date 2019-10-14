{-# LANGUAGE OverloadedStrings #-}

module Dhaml.Internal
    ( ParseState(..)
    , Stack
    , evaluateTag
    , initialState
    , run
    , withAnchor
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.ByteString.Char8 (ByteString)
import Data.Conduit (ConduitT, awaitForever, runConduit, yield)
import Data.Conduit.List (sourceList)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Libyaml (Event(..))
import qualified Text.Libyaml as LY

-- | Type synonym for the monad transformer stack
type Stack = StateT ParseState (ResourceT IO)

-- | State available to the monad transformer stack
data ParseState = ParseState
    { hasErrors :: Bool
    }

-- | The initial state
initialState :: ParseState
initialState = ParseState False

-- | Run the conduit to encode a stream of 'Event's as a 'ByteString'
run :: ConduitT () Void Stack ByteString -> IO ByteString
run conduit = do
    result <- runResourceT $ runStateT (runConduit conduit) initialState
    if hasErrors $ snd result
        then exitFailure
        else return $ fst result

-- | A conduit for evaluating tagged 'EventScalar's using the specified handler
evaluateTag :: String -> a -> (a -> ByteString -> Stack [Event]) -> ConduitT Event Event Stack ()
evaluateTag tag ctx handler = awaitForever $ \event -> do
    case event of
        EventScalar expr t _ anchor ->
            if t == (LY.UriTag tag) then do
                events <- lift $ handler ctx expr
                sourceList $ withAnchor anchor events
            else
                yield event
        _ ->
            yield event

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

