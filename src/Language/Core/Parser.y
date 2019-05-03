{
module Language.Core.Parser ( parser ) where    

import Lexer (Token(..))
import ApiAnnotation (IsUnicodeSyntax(..))
import Language.Core.Syntax
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

}
