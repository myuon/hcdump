module Main where

import qualified Data.ByteString as B
import Options.Applicative
import qualified GHC
import qualified GHC.Paths
import qualified Outputable

data Arg = Arg {
  filepath :: FilePath
} deriving (Eq, Show)

argParser :: Parser Arg
argParser =
  Arg <$> strOption (long "input" <> short 'i' <> help "Path to the source")

main :: IO ()
main = runCLI =<< execParser opts
 where
  opts = info
    (argParser <**> helper)
    ( fullDesc <> progDesc "Analyze the dumped GHC Core program." <> header
      "core-dump (ddc) - GHC Core analyzer"
    )

runCLI :: Arg -> IO ()
runCLI arg = do
  src <- B.readFile (filepath arg)
  B.putStrLn src
