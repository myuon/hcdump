module Language.Core.Lexer (
  Lexer.Token(..),
  Lexer.ParseResult(..),
  getDynFlags,
  lexTokenStream,
  lexTokenStreamWith,
) where

import qualified Data.ByteString as B
import qualified Data.StringBuffer as SB
import qualified DynFlags
import qualified GHC
import qualified GHC.LanguageExtensions.Type as LangExt
import qualified GHC.Paths
import qualified Lexer
import qualified SrcLoc

getDynFlags :: IO (GHC.DynFlags)
getDynFlags = GHC.runGhc (Just GHC.Paths.libdir) $ do
  dflags <- GHC.getSessionDynFlags
  let dflags' = DynFlags.xopt_set dflags LangExt.MagicHash
  return dflags'

lexTokenStream
  :: B.ByteString -> IO (Lexer.ParseResult [GHC.Located Lexer.Token])
lexTokenStream buf = do
  dflags <- getDynFlags
  lexTokenStreamWith dflags buf

lexTokenStreamWith
  :: GHC.DynFlags
  -> B.ByteString
  -> IO (Lexer.ParseResult [GHC.Located Lexer.Token])
lexTokenStreamWith dflags buf = do
  return $ Lexer.lexTokenStream (SB.fromByteString buf)
                                (SrcLoc.mkRealSrcLoc "" 0 0)
                                dflags
