{
module Language.Core.Parser (
  parser,
  parseByteString,
) where    

import ApiAnnotation (IsUnicodeSyntax(..))
import qualified Data.ByteString as B
import Language.Core.Lexer
import Language.Core.Syntax
import qualified SrcLoc
}

%name parser
%tokentype { Token }

%token
    '::'    { ITdcolon NormalSyntax }
    '['     { ITobrack }
    ']'     { ITcbrack }
    '='     { ITequal }
    VAR     { ITvarid $$ }
    LCOMMENT    { ITlineComment $$ }

%%

bind    :: { Bind Var }
bind    : VAR typedecl      { NonRec $1 (Func $2 "" (Var $1)) }

typedecl    :: { Type }
typedecl    : '::' VAR      { TyVarTy $2 }

{
happyError tokens = error $ "Parse error\n" ++ show (take 10 tokens)

parseByteString :: B.ByteString -> IO (Bind Var)
parseByteString buf = do
  result <- lexTokenStream buf

  case result of
    POk _ v -> return $ parser (map SrcLoc.unLoc v)
}
