module Language.Core.Syntax where

import Prelude hiding ((<>))
import FastString (FastString)

data DumpSimpl
  = DumpSimpl ModuleStat Decls
  deriving (Eq, Show)

type ModuleStat = String

-- We define @Type@ and some other things here, because they are not exported in ghc library.
data Decls = Decls [Bind Var]
  deriving (Eq, Show)

data Bind b
  = NonRec b (Func b)
  | Rec [(b, Func b)]
  deriving (Eq, Show)

data Func b
  = Func Type FuncStat (Expr b)
  deriving (Eq, Show)

type FuncStat = FastString
type KindOrType = Type
type TyVarBinder = ()
type TyCon = ()

data Type
  = TyVarTy Var
  | AppTy Type Type
  | TyConApp TyCon [KindOrType]
  | ForallTy TyVarBinder Type
  | FunTy Type Type
  deriving (Eq, Show)

data Var
  = Token FastString
  | QToken FastString FastString
  deriving (Eq, Show)

type Id = Var
type Coercion = Type
type Arg b = Expr b
type Alt b = ()
type Tickish b = ()
type Literal = ()

data Expr b
  = Var Id
  | Lit Literal
  | App (Expr b) (Arg b)
  | Lam b (Expr b)
  | Let (Bind b) (Expr b)
  | Case (Expr b) b Type [Alt b]
  | Cast (Expr b) Coercion
  | Tick (Tickish Id) (Expr b)
  | Coercion Coercion
  deriving (Eq, Show)
