{
module Language.Core.Parser (
  parser,
  parseByteString,
) where    

import ApiAnnotation (IsUnicodeSyntax(..))
import qualified Data.ByteString as B
import Language.Core.Lexer
import Language.Core.Syntax
import FastString (FastString)
import qualified GHC
import qualified GHC.Paths
import qualified Outputable
import qualified SrcLoc
}

%name parser
%tokentype { Token }
%monad { Either String } { (>>=) } { return }

%token
    '::'    { ITdcolon NormalSyntax }
    '['     { ITobrack }
    ']'     { ITcbrack }
    '='     { ITequal }
    ','     { ITcomma }

    GblId   { ITconid "GblId" }
    LclIdX  { ITconid "LclIdX" }
    LclId   { ITconid "LclId" }
    OtherCon   { ITconid "OtherCon" }

    Caf     { ITconid "Caf" }
    Unf     { ITconid "Unf" }
    Str     { ITconid "Str" }

    VAR     { ITvarid $$ }
    qVAR    { ITqvarid $$ }
    CON     { ITconid $$ }
    qCON    { ITqconid $$ }
    LCOMMENT    { ITlineComment $$ }
    PSTRING { ITprimstring _ $$ }
    PINTEGER { ITprimint _ $$ }

%%

bind    :: { Bind Var }
bind    : LCOMMENT
        VAR typedecl
        id_info
        VAR '=' expr
        { NonRec (Token $2) (Func $3 $4 $7) }

id_info   :: { IdInfo }
id_info   : '[' id_info_list ']'       { $2 }

id_info_list  :: { [(FastString, FastString)] }
id_info_list  : id_info_item     { [$1] }
              | id_info_item ',' id_info_list   { $1 : $3 }

id_info_item  :: { (FastString, FastString) }
id_info_item  : GblId     { ("IdType", "GlobalId") }
              | LclIdX    { ("IdType", "ExportedId") }
              | LclId     { ("IdType", "LocalId") }
              | Caf '=' CON   { ("Caf", $3) }
              | Str '=' VAR   { ("Str", $3) }
              | Unf '=' OtherCon '[' ']'    { ("Unf", "OtherCon []") }

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
expr    : expr expr_terminal { App $1 $2 }
        | expr_terminal      { $1 }

expr_terminal   :: { Expr Var }
expr_terminal   : var       { Var $1 }
                | con       { Var $1 }
                | PSTRING   { Lit (MachStr $1) }
                | PINTEGER  { Lit (LitNumber $1) }

{
happyError tokens = Left $ "Parse error\n" ++ show (take 10 tokens)

parseByteString :: B.ByteString -> IO (Either String (Bind Var))
parseByteString buf = do
  result <- lexTokenStream buf
  dflags <- GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags

  case result of
    POk _ v -> return $ parser (map SrcLoc.unLoc v)
    PFailed _ _ md -> return $ Left $ Outputable.renderWithStyle
        dflags
        md
        (Outputable.defaultErrStyle dflags)
}
