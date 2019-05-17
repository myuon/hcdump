{
module Language.Core.Parser (
  parser,
  parseByteString,
  parseByteStringWith,
) where

import ApiAnnotation (IsUnicodeSyntax(..))
import BasicTypes (IntegralLit(IL))
import qualified Data.ByteString as B
import Language.Core.Lexer
import Language.Core.Syntax
import FastString (FastString)
import qualified FastString as FS
import qualified GHC
import qualified GHC.Paths
import qualified Outputable
import qualified SrcLoc
}

%name parser
%tokentype { SrcLoc.GenLocated SrcLoc.SrcSpan Token }
%monad { Either String } { (>>=) } { return }

%token
    '<0>['  { SrcLoc.L (SrcLoc.RealSrcSpan l) (ITobrack) | SrcLoc.srcSpanStartCol l == 1 }
    '<0>:'  { SrcLoc.L (SrcLoc.RealSrcSpan l) (ITcolon) | SrcLoc.srcSpanStartCol l == 1 }
    '::'    { SrcLoc.L _ (ITdcolon NormalSyntax) }
    '['     { SrcLoc.L _ (ITobrack) }
    ']'     { SrcLoc.L _ (ITcbrack) }
    '{'     { SrcLoc.L _ (ITocurly) }
    '}'     { SrcLoc.L _ (ITccurly) }
    '('     { SrcLoc.L _ (IToparen) }
    ')'     { SrcLoc.L _ (ITcparen) }
    '='     { SrcLoc.L _ (ITequal) }
    ','     { SrcLoc.L _ (ITcomma) }
    '@'     { SrcLoc.L _ (ITat) }
    '`'     { SrcLoc.L _ (ITbackquote) }
    '$w/w'  { SrcLoc.L _ (ITqvarsym ($$,"$")) }

    Caf     { SrcLoc.L _ (ITconid "Caf") }
    Unf     { SrcLoc.L _ (ITconid "Unf") }
    Str     { SrcLoc.L _ (ITconid "Str") }

    GblId     { SrcLoc.L _ (ITconid "GblId") }
    LclIdX    { SrcLoc.L _ (ITconid "LclIdX") }
    LclId     { SrcLoc.L _ (ITconid "LclId") }
    Arity     { SrcLoc.L _ (ITconid "Arity") }
    OtherCon  { SrcLoc.L _ (ITconid "OtherCon") }
    Src       { SrcLoc.L _ (ITconid "Src") }
    TopLvl    { SrcLoc.L _ (ITconid "TopLvl") }
    Value     { SrcLoc.L _ (ITconid "Value") }
    ConLike   { SrcLoc.L _ (ITconid "ConLike") }
    WorkFree  { SrcLoc.L _ (ITconid "WorkFree") }
    Expandable  { SrcLoc.L _ (ITconid "Expandable") }
    Guidance  { SrcLoc.L _ (ITconid "Guidance") }
    Tmpl      { SrcLoc.L _ (ITconid "Tmpl") }

    ALWAYS_IF   { SrcLoc.L _ (ITconid "ALWAYS_IF") }

    CAST        { SrcLoc.L _ (ITvarid "cast") }

    VAR         { SrcLoc.L _ (ITvarid $$) }
    qVAR        { SrcLoc.L _ (ITqvarid $$) }
    CON         { SrcLoc.L _ (ITconid $$) }
    qCON        { SrcLoc.L _ (ITqconid $$) }
    LCOMMENT    { SrcLoc.L _ (ITlineComment $$) }
    PSTRING     { SrcLoc.L _ (ITprimstring _ $$) }
    PINTEGER    { SrcLoc.L _ (ITprimint _ $$) }
    INT         { SrcLoc.L _ (ITinteger (IL _ _ $$)) }

%%

bind    :: { Bind Var }
bind    : LCOMMENT
        bind_name typedecl
        id_info
        var '=' expr
        { NonRec $2 (Func $3 $4 $7) }

bind_name   :: { Var }
bind_name   : var           { $1 }
            | '<0>:' var    { colonVar $2 }

id_info   :: { IdInfo }
id_info   : '<0>[' id_info_list ']'       { IdInfo $2 }

id_info_list  :: { [(FastString, FastString)] }
id_info_list  : id_info_item     { [$1] }
              | id_info_item ',' id_info_list   { $1 : $3 }

id_info_item  :: { (FastString, FastString) }
id_info_item  : GblId     { ("IdType", "GlobalId") }
              | LclIdX    { ("IdType", "ExportedId") }
              | LclId     { ("IdType", "LocalId") }
              | Caf '=' CON   { ("Caf", $3) }
              | Str '=' VAR   { ("Str", $3) }
              | Unf '=' unf   { ("Unf", $3) }
              | Arity '=' INT     { ("Arity", FS.fsLit $ show $3) }

unf     :: { FastString }
unf     : OtherCon '[' ']'    { "OtherCon []" }
        | Unf
        '{' Src '=' CON
        ',' TopLvl '=' CON
        ',' Value '=' CON
        ',' ConLike '=' CON
        ',' WorkFree '=' CON
        ',' Expandable '=' CON
        ',' Guidance '=' uf_guidance
        Tmpl '=' type
        '}'
        { "CoreUnfolding {...}" }

uf_guidance   : ALWAYS_IF '(' key_value_eq ')'
              { "UnfIfGoodArgs {" <> FS.fsLit (show $3) <> "}" }

key_value_eq    :: { [(FastString, Expr Var)] }
                : VAR '=' expr                      { ($1,$3) : [] }
                | VAR '=' expr ',' key_value_eq     { ($1,$3) : $5 }


var     :: { Var }
var     : VAR       { Token $1 }
        | qVAR      { uncurry QToken $1 }

con     :: { Var }
con     : CON       { Token $1 }
        | qCON      { uncurry QToken $1 }

type        :: { Type }
type        : '(' type ')'          { $2 }
            | type type_terminal    { AppTy $1 $2 }
            | type_terminal         { $1 }

type_terminal   :: { Type }
type_terminal   : var           { TyVarTy $1 }
                | con           { TyConApp $1 [] }
                | '[' type ']'  { TyConApp (Token "List") [$2] }
                | '(' ')'       { TyConApp (Token "Unit") [] }

typedecl    :: { Type }
typedecl    : '::' type      { $2 }

coercion    :: { Coercion }
coercion    : expr '::' type    { Coercion $1 $3 }

expr    :: { Expr Var }
expr    : '(' expr ')'              { $2 }
        | expr_terminal '@' type    { App $1 (Type $3) }
        | expr_terminal '`' CAST '`' coercion    { Cast $1 $5 }
        | expr_terminal             { $1 }
        | expr expr_list            { foldl App $1 $2 }

expr_list     : expr expr_list  { $1 : $2 }
              | expr            { [$1] }

expr_terminal   :: { Expr Var }
expr_terminal   : '$w/w' var    { Var (wwVar $1 $2) }
                | var           { Var $1 }
                | con           { Var $1 }
                | PSTRING       { Lit (MachStr $1 True) }
                | PINTEGER      { Lit (LitNumber $1 True) }
                | INT           { Lit (LitNumber $1 False) }

{
wwVar :: FastString -> Var -> Var
wwVar q (Token v) = QToken q ("$" <> v)

colonVar :: Var -> Var
colonVar (QToken q v) = QToken q (":" <> v)

happyError tokens = Left $ "Parse error\n" ++ show (take 10 $ map SrcLoc.unLoc tokens)

-- instance (Show l, Show e) => Show (SrcLoc.GenLocated l e) where
--   show (SrcLoc.L l e) = "L (" ++ show l ++ ") (" ++ show e ++ ")"

parseByteStringWith :: GHC.DynFlags -> B.ByteString -> IO (Either String (Bind Var))
parseByteStringWith dflags buf = do
  result <- lexTokenStream buf

  case result of
    POk _ v -> do
--      print $ map SrcLoc.unLoc v
      return $ parser v
    PFailed _ _ md -> return $ Left $ Outputable.renderWithStyle
        dflags
        md
        (Outputable.defaultErrStyle dflags)

parseByteString :: B.ByteString -> IO (Either String (Bind Var))
parseByteString buf = do
  dflags <- getDynFlags
  parseByteStringWith dflags buf
}
