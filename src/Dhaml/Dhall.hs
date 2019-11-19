{-# LANGUAGE OverloadedStrings #-}

module Dhaml.Dhall
    ( Context(..)
    , exprToEvents
    , handler
    , inputExpr
    , toValue
    ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (get)
import Data.Aeson (Value)
import Data.Aeson.Types (JSONPath, formatPath)
import Data.ByteString.Char8 (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import Data.Yaml.Internal (defaultStringStyle, objToEvents)
import Dhall (inputExpr)
import Dhall.Core hiding (path, value)
import Dhall.Import (SemanticCacheMode(..), loadRelativeTo)
import qualified Dhall.JSON as DJ
import Dhall.Parser (Src, exprFromText)
import Dhall.TypeCheck (typeOf)
import Text.Libyaml (Event(..))

import Dhaml.Internal

data Context = Context
    { dir   :: FilePath
    , input :: Maybe (Expr Src Void)
    }

-- | Convenience function for wrapping and throwing an exception
eitherThrow :: (Exception e, Show a) => (String -> e) -> Either a b -> IO b
eitherThrow e (Left  a) = throwIO . e $ show a
eitherThrow _ (Right b) = return b

-- | Evaluate a Dhall expression and convert it into a list of 'Event's
handler :: Context -> ByteString -> Stack [Event]
handler ctx expr = do
    state <- get
    liftIO . exprToEvents ctx expr $ path state

-- | Convert a Dhall 'Expr' from a 'ByteString' into a list of 'Event's
exprToEvents :: Context -> ByteString -> JSONPath -> IO [Event]
exprToEvents context bs exprPath = do
    let formattedPath = "\ESC[1;36m" ++ formatPath exprPath ++ "\ESC[0m"
    expr1 <- eitherThrow ExprError . exprFromText formattedPath $ decodeUtf8 bs
    expr2 <- loadRelativeTo (dir context) UseSemanticCache $ expr1
    let expr3 = maybe expr2 (\x -> subst (V "x" 0) x expr2) $ input context
    _     <- eitherThrow ExprError $ typeOf expr3
    value <- toValue $ normalize expr3
    return $ objToEvents defaultStringStyle value []

-- | Convert a Dhall 'Expr' into an Aeson 'Value'
toValue :: Expr Src Void -> IO Value
toValue expr = do
    let expr1 = DJ.convertToHomogeneousMaps (DJ.Conversion "mapKey" "mapValue") expr
    expr2 <- eitherThrow ExprError $ DJ.handleSpecialDoubles DJ.UseYAMLEncoding expr1
    eitherThrow ExprError $ DJ.dhallToJSON expr2
