module Language.Core.Syntax (
  DumpSimpl (..),
  ModuleStat (..),
  Decls (..),
  Bind (..),
  Func (..),
  FuncStat (..),
  KindOrType (..),
  Type (..),
  Id,
  Coercion,
  Arg,
  Expr (..),
  Var (..),
) where

import FastString (FastString)
import qualified Lexer
import qualified Literal
import qualified CoreSyn
import Var (TyVarBinder)
import TyCon (TyCon (..))

data DumpSimpl = DumpSimpl ModuleStat Decls
type ModuleStat = String

-- We define @Type@ and some other things here, because they are not exported in ghc library.
data Decls = Decls [Bind Var]

data Bind b
  = NonRec b (Func b)
  | Rec [(b, Func b)]

data Func b
  = Func Type FuncStat (Expr b)

type FuncStat = FastString
type KindOrType = Type

data Type
  = TyVarTy Var
  | AppTy Type Type
  | TyConApp TyCon [KindOrType]
  | ForallTy {-# UNPACK #-} !TyVarBinder Type
  | FunTy Type Type

type Var = FastString
type Id = Var
type Coercion = Type
type Arg b = Expr b

data Expr b
  = Var Id
  | Lit Literal.Literal
  | App (Expr b) (Arg b)
  | Lam b (Expr b)
  | Let (Bind b) (Expr b)
  | Case (Expr b) b Type [CoreSyn.Alt b]
  | Cast (Expr b) Coercion
  | Tick (CoreSyn.Tickish Id) (Expr b)
  | Coercion Coercion
