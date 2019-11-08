{-# LANGUAGE OverloadedStrings #-}

module Dhaml.Dhall
    ( Context(..)
    , DhamlException(..)
    , exprToEvents
    , handler
    , inputExpr
    , toValue
    ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (modify)
import Data.Aeson (Value)
import Data.ByteString.Char8 (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml.Internal (defaultStringStyle, objToEvents)
import Dhall (inputExpr)
import Dhall.Core hiding (value)
import Dhall.Import (SemanticCacheMode(..), loadRelativeTo)
import qualified Dhall.JSON as DJ
import Dhall.Parser (Src, exprFromText)
import Dhall.TypeCheck (X, typeOf)
import System.IO (hPutStrLn, stderr)
import Text.Libyaml (Event(..))

import Dhaml.Internal

data DhamlException
    = DhamlJSONError  String
    | DhamlParseError String
    | DhamlTypeError  String

instance Show DhamlException where
    show (DhamlJSONError  s) = s
    show (DhamlParseError s) = s
    show (DhamlTypeError  s) = s

instance Exception DhamlException

data Context = Context
    { dir   :: FilePath
    , input :: Maybe (Expr Src X)
    }

-- | Convenience function for wrapping and throwing an exception
eitherThrow :: (Exception e, Show a) => (String -> e) -> Either a b -> IO b
eitherThrow e (Left  a) = throwIO . e $ show a
eitherThrow _ (Right b) = return b

-- | Evaluate a Dhall expression and convert it into a list of 'Event's
handler :: Context -> ByteString -> Stack [Event]
handler ctx expr =
    catch (liftIO $ exprToEvents ctx expr) logError

-- | Print any errors as the YAML file is parsed and set the state accordingly
logError :: DhamlException -> Stack [Event]
logError e = do
    liftIO . hPutStrLn stderr $ show e
    modify $ \x -> x { hasErrors = True }
    return []

-- | Convert a Dhall 'Expr' from a 'ByteString' into a list of 'Event's
exprToEvents :: Context -> ByteString -> IO [Event]
exprToEvents context bs = do
    expr1 <- eitherThrow DhamlParseError . exprFromText "(expression)" $ decodeUtf8 bs
    expr2 <- loadRelativeTo (dir context) UseSemanticCache $ expr1
    let expr3 = maybe expr2 (\x -> subst (V "x" 0) x expr2) $ input context
    _     <- eitherThrow DhamlTypeError $ typeOf expr3
    value <- toValue $ normalize expr3
    return $ objToEvents defaultStringStyle value []

-- | Convert a Dhall 'Expr' into an Aeson 'Value'
toValue :: Expr Src X -> IO Value
toValue expr = do
    let expr1 = DJ.convertToHomogeneousMaps (DJ.Conversion "mapKey" "mapValue") expr
    expr2 <- eitherThrow DhamlJSONError $ DJ.handleSpecialDoubles DJ.UseYAMLEncoding expr1
    eitherThrow DhamlJSONError $ DJ.dhallToJSON expr2
