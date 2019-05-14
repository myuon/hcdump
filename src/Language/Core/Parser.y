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
import FastString (FastString)
}

%name parser
%tokentype { Token }

%token
    '::'    { ITdcolon NormalSyntax }
    '['     { ITobrack }
    ']'     { ITcbrack }
    '='     { ITequal }
    ','     { ITcomma }

    GblId   { ITconid "GblId" }
    Caf     { ITconid "Caf" }
    Unf     { ITconid "Unf" }

    VAR     { ITvarid $$ }
    CON     { ITconid $$ }
    qCON    { ITqconid $$ }
    LCOMMENT    { ITlineComment $$ }
    PSTRING { ITprimstring _ $$ }

%%

bind    :: { Bind Var }
bind    : LCOMMENT
        VAR typedecl
        bstat
        VAR '=' body
        { NonRec (Token $2) (Func $3 $4 $7) }

bstat   :: { FastString }
bstat   : '[' GblId ']'       { "" }
        | '[' GblId ',' Caf '=' CON ',' Unf '=' CON '[' ']' ']'     { $6 }

typedecl    :: { Type }
typedecl    : '::' qCON      { TyVarTy (uncurry QToken $2) }

body    :: { Expr Var }
body    : PSTRING   { Lit () }

{
happyError tokens = error $ "Parse error\n" ++ show (take 10 tokens)

parseByteString :: B.ByteString -> IO (Bind Var)
parseByteString buf = do
  result <- lexTokenStream buf

  case result of
    POk _ v -> return $ parser (map SrcLoc.unLoc v)
}
