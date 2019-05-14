module Language.Core.Lexer where

import qualified DynFlags
import qualified GHC
import qualified GHC.LanguageExtensions.Type as LangExt
import qualified GHC.Paths
import qualified Lexer
import qualified StringBuffer
import qualified SrcLoc

lexTokenStream
  :: StringBuffer.StringBuffer
  -> IO (Lexer.ParseResult [GHC.Located Lexer.Token])
lexTokenStream buf = do
  dflags <- GHC.runGhc (Just GHC.Paths.libdir) $ do
    dflags <- GHC.getSessionDynFlags

    let dflags' = DynFlags.xopt_set dflags LangExt.MagicHash
    _ <- GHC.setSessionDynFlags dflags'
    return dflags'

  return $ Lexer.lexTokenStream buf (SrcLoc.mkRealSrcLoc "" 0 0) dflags
