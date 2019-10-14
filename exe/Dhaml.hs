{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Data.Conduit ((.|))
import Data.Text.Encoding (decodeUtf8)
import Options.Applicative
import System.FilePath.Posix (takeDirectory)
import System.IO (char8, hSetEncoding, stderr, stdout)
import qualified Text.Libyaml as LY

import qualified Dhaml.Dhall as Dhall
import Dhaml.Internal

data Options = Options
    { file  :: FilePath
    , input :: Bool
    }

parser :: Parser Options
parser = Options
    <$> argument str
        (  metavar "FILE"
        <> help "YAML file containing Dhall expressions"
        )
    <*> switch
        (  long "input"
        <> short 'i'
        <> help "Bind the expression from stdin to x"
        )

main :: IO ()
main = do
    hSetEncoding stdout char8
    hSetEncoding stderr char8

    options <- execParser $ info (parser <**> helper) fullDesc

    bindings <-
        if input options then do
            expr <- decodeUtf8 <$> BS.getContents
            Just <$> Dhall.inputExpr expr
        else
            return Nothing

    let ctx = Dhall.Context (takeDirectory $ file options) bindings
    let conduit =
               LY.decodeFile (file options)
            .| evaluateTag "!dhall" ctx Dhall.handler
            .| LY.encode

    yaml <- run conduit
    putStr $ BS.unpack yaml
