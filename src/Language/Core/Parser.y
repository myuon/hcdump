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
    qVAR    { ITqvarid $$ }
    CON     { ITconid $$ }
    qCON    { ITqconid $$ }
    LCOMMENT    { ITlineComment $$ }
    PSTRING { ITprimstring _ $$ }

%%

bind    :: { Bind Var }
bind    : LCOMMENT
        VAR typedecl
        bstat
        VAR '=' expr
        { NonRec (Token $2) (Func $3 $4 $7) }

bstat   :: { FastString }
bstat   : '[' GblId ']'       { "" }
        | '[' GblId ',' Caf '=' CON ',' Unf '=' CON '[' ']' ']'     { $6 }

var     :: { Var }
var     : VAR       { Token $1 }
        | qVAR      { uncurry QToken $1 }

con     :: { Var }
con     : CON       { Token $1 }
        | qCON      { uncurry QToken $1 }

type        :: { Type }
type        : var       { TyVarTy $1 }
            | con       { TyConApp $1 [] }
            | '[' type ']'  { TyConApp (Token "List") [$2] }

typedecl    :: { Type }
typedecl    : '::' type      { $2 }

expr    :: { Expr Var }
expr    : var       { Var $1 }
        | PSTRING   { Lit () }
        | expr expr { App $1 $2 }

{
happyError tokens = error $ "Parse error\n" ++ show (take 10 tokens)

parseByteString :: B.ByteString -> IO (Bind Var)
parseByteString buf = do
  result <- lexTokenStream buf

  case result of
    POk _ v -> return $ parser (map SrcLoc.unLoc v)
}
