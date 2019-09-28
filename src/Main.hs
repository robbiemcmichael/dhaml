{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (Exception, throwIO)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Trans.State (StateT, modify, runStateT)
import Data.Aeson (Value)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)
import Data.Conduit (ConduitT, awaitForever, runConduit, yield, (.|))
import Data.Conduit.List (sourceList)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Yaml as Y
import qualified Dhall as D
import qualified Dhall.Core as D
import qualified Dhall.Import as D
import qualified Dhall.JSON as D
import qualified Dhall.Parser as D
import qualified Dhall.TypeCheck as D
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix (takeDirectory)
import System.IO (char8, hPutStrLn, hSetEncoding, stderr, stdout)
import Text.Libyaml (Event(..))
import qualified Text.Libyaml as LY

type M = StateT ParseState (ResourceT IO)

data ParseState = ParseState
    { hasErrors :: Bool
    }

initialState :: ParseState
initialState = ParseState False

data DhamlException
    = DhamlJSONError  String
    | DhamlParseError String
    | DhamlTypeError  String

instance Show DhamlException where
    show (DhamlJSONError  s) = s
    show (DhamlParseError s) = s
    show (DhamlTypeError  s) = s

instance Exception DhamlException

eitherThrow :: (Exception e, Show a) => (String -> e) -> Either a b -> IO b
eitherThrow e (Left  a) = throwIO . e $ show a
eitherThrow _ (Right b) = return b

main :: IO ()
main = do
    hSetEncoding stdout char8
    hSetEncoding stderr char8

    args <- getArgs
    file <- case args of
        [x] -> return x
        _   -> hPutStrLn stderr "usage: dhaml yaml-file" >> exitFailure

    dhall <- decodeUtf8 <$> BS.getContents
    expr  <- if T.all isSpace dhall
        then return Nothing
        else Just <$> D.inputExpr dhall

    yaml  <- encode $ LY.decodeFile file .| processEvents (takeDirectory file) expr
    putStr $ BS.unpack yaml

-- | Process events and evaluate any Dhall expressions
processEvents :: FilePath -> Maybe (D.Expr D.Src D.X) -> ConduitT Event Event M ()
processEvents filepath input = awaitForever $ \event -> do
    case event of
        EventScalar expr (LY.UriTag "!dhall") _ _ -> do
            events <- lift $ catch (liftIO $ dhallToEvents filepath input expr) (handler event)
            sourceList events
        _ ->
            yield event

-- | Print any errors as the YAML file is being parsed
handler :: MonadIO m => Event -> DhamlException -> StateT ParseState m [Event]
handler originalEvent e = do
    liftIO . hPutStrLn stderr $ show e
    modify $ \x -> x { hasErrors = True }
    return [originalEvent]

-- | Convert a Dhall 'Expr' from a 'ByteString' into a stream of 'Event's
dhallToEvents :: FilePath -> Maybe (D.Expr D.Src D.X) -> ByteString -> IO [Event]
dhallToEvents filepath input bs = do
    expr1 <- eitherThrow DhamlParseError . D.exprFromText "(expression)" $ decodeUtf8 bs
    expr2 <- D.loadRelativeTo filepath D.UseSemanticCache $ expr1
    let expr3 = maybe expr2 (\x -> D.subst (D.V "x" 0) x expr2) input
    _     <- eitherThrow DhamlTypeError $ D.typeOf expr3
    expr4 <- exprToValue $ D.normalize expr3
    return $ Y.objToEvents Y.defaultEncodeOptions expr4 []

-- | Convert a Dhall 'Expr' into an Aeson 'Value'
exprToValue :: D.Expr s D.X -> IO Value
exprToValue expr = do
    let expr1 = D.convertToHomogeneousMaps (D.Conversion "mapKey" "mapValue") expr
    expr2 <- eitherThrow DhamlJSONError $ D.handleSpecialDoubles D.UseYAMLEncoding expr1
    eitherThrow DhamlJSONError $ D.dhallToJSON expr2

-- | Encode a stream of 'Event's as a 'ByteString'
encode :: ConduitT () Event M () -> IO ByteString
encode events = do
    result <- runResourceT $ runStateT (runConduit $ events .| LY.encode) initialState
    if hasErrors $ snd result
        then exitFailure
        else return $ fst result
