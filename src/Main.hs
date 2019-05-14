{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Fix
import Language.Core.Lexer (lexTokenStream)
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
      "core-dump (hcdump) - GHC Core analyzer"
    )

stripHeader :: StringBuffer.StringBuffer -> StringBuffer.StringBuffer
stripHeader = fix
  ( \f buf ->
    let Just line0 = StringBuffer.atLine 1 buf
        len0       = StringBuffer.cur line0
    in  ( if eqBuffer
             line0
             ( StringBuffer.stringToStringBuffer
               "==================== Tidy Core ====================\n"
             )
          then id
          else f
        )
          $ StringBuffer.offsetBytes len0 buf
  )
 where
  eqBuffer :: StringBuffer.StringBuffer -> StringBuffer.StringBuffer -> Bool
  eqBuffer buf1 buf2
    | StringBuffer.atEnd buf1 && StringBuffer.atEnd buf2
    = True
    | otherwise
    = let (b1, buf1') = StringBuffer.nextChar buf1
          (b2, buf2') = StringBuffer.nextChar buf1
      in  b1 == b2 && eqBuffer buf1' buf2'

dropWhileSB
  :: (Char -> Bool) -> StringBuffer.StringBuffer -> StringBuffer.StringBuffer
dropWhileSB f buf =
  let (b, bs) = StringBuffer.nextChar buf
  in  if f b then dropWhileSB f bs else buf

runCLI :: Arg -> IO ()
runCLI arg = do
  filebuf <- fmap (dropWhileSB (/= '-'))
    $ StringBuffer.hGetStringBuffer (filepath arg)

  dflags <- GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags
  result <- lexTokenStream filebuf

  case result of
    Lexer.POk _ v -> do
      print $ map SrcLoc.unLoc v
      putStrLn $ Outputable.showSDoc dflags $ Outputable.ppr $ parser
        (map SrcLoc.unLoc v)
