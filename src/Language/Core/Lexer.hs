module Language.Core.Lexer (
  Lexer.Token(..),
  Lexer.ParseResult(..),
  lexTokenStream,
) where

import qualified Data.ByteString as B
import qualified Data.StringBuffer as SB
import qualified DynFlags
import qualified GHC
import qualified GHC.LanguageExtensions.Type as LangExt
import qualified GHC.Paths
import qualified Lexer
import qualified SrcLoc

lexTokenStream
  :: B.ByteString -> IO (Lexer.ParseResult [GHC.Located Lexer.Token])
lexTokenStream buf = do
  dflags <- GHC.runGhc (Just GHC.Paths.libdir) $ do
    dflags <- GHC.getSessionDynFlags

    let dflags' = DynFlags.xopt_set dflags LangExt.MagicHash
    _ <- GHC.setSessionDynFlags dflags'
    return dflags'

  return $ Lexer.lexTokenStream (SB.fromByteString buf)
                                (SrcLoc.mkRealSrcLoc "" 0 0)
                                dflags
