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

simpl       :: { DumpSimpl }
simpl       : modstat decls         { DumpSimpl $1 (Decls $2) }

modstat     :: { ModuleStat }
modstat     : {- empty -}       { "" }

decls       :: { [Bind Var] }
decls       : {- empty -}       { [] }
            | decl decls     { $1 : $2 }

decl        :: { Bind Var }
decl        : func      { $1 }

func        :: { Bind Var }
func        : LCOMMENT
                VAR '::' type
                func_stat
                VAR '=' body
                { NonRec $2 (Func $4 $5 $8) }

type        :: { Type }
type        : VAR       { TyVarTy $1 }

func_stat   :: { FuncStat }
func_stat   : '[' VAR ']'   { $2 }

body        :: { Expr Var }
body        : VAR       { Var $1 }

{
happyError tokens = error $ "Parse error\n" ++ show (take 10 tokens)

parseByteString :: B.ByteString -> IO DumpSimpl
parseByteString buf = do
  result <- lexTokenStream buf

  case result of
    POk _ v -> return $ parser (map SrcLoc.unLoc v)
}
