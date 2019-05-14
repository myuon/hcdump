{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Fix
import qualified Data.ByteString as B
import qualified Data.StringBuffer as SB
import Language.Core.Lexer (lexTokenStream)
import Language.Core.Parser (parser)
import Options.Applicative
import qualified Lexer
import qualified SrcLoc

data Arg = Arg {
  filepath :: Maybe FilePath
} deriving (Eq, Show)

argParser :: Parser Arg
argParser = Arg <$> optional
  (strOption (long "input" <> short 'i' <> help "Path to the source"))

main :: IO ()
main = runCLI =<< execParser opts
 where
  opts = info
    (argParser <**> helper)
    ( fullDesc <> progDesc "Analyze the dumped GHC Core program." <> header
      "core-dump (hcdump) - GHC Core analyzer"
    )

stripHeader :: SB.StringBuffer -> SB.StringBuffer
stripHeader = fix
  ( \f buf ->
    let Just line0 = SB.atLine 1 buf
        len0       = SB.cur line0
    in  ( if eqBuffer
             line0
             ( SB.stringToStringBuffer
               "==================== Tidy Core ====================\n"
             )
          then id
          else f
        )
          $ SB.offsetBytes len0 buf
  )
 where
  eqBuffer :: SB.StringBuffer -> SB.StringBuffer -> Bool
  eqBuffer buf1 buf2
    | SB.atEnd buf1 && SB.atEnd buf2
    = True
    | otherwise
    = let (b1, buf1') = SB.nextChar buf1
          (b2, buf2') = SB.nextChar buf1
      in  b1 == b2 && eqBuffer buf1' buf2'

runCLI :: Arg -> IO ()
runCLI arg = do
  buf <- case filepath arg of
    Nothing -> B.getContents
    Just path ->
      fmap (SB.toByteString . SB.dropWhile (/= '-')) $ SB.hGetStringBuffer path
  result <- lexTokenStream buf

  case result of
    Lexer.POk _ v -> do
      print $ map SrcLoc.unLoc v
      print $ parser (map SrcLoc.unLoc v)
