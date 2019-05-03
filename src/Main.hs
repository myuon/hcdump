{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import Language.Core.Parser (parser)
import Options.Applicative
import qualified Lexer
import qualified GHC
import qualified GHC.Paths
import qualified Outputable
import qualified SrcLoc
import qualified StringBuffer

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
  dflags  <- GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags
  filebuf <- StringBuffer.hGetStringBuffer (filepath arg)

  let result = Lexer.lexTokenStream filebuf (SrcLoc.mkRealSrcLoc "" 0 0) dflags
  case result of
    Lexer.POk _ v ->
      putStrLn $ Outputable.showSDoc dflags $ Outputable.ppr $ parser
        (map SrcLoc.unLoc v)
