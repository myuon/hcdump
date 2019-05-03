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

import Prelude hiding ((<>))
import FastString (FastString)
import qualified Lexer
import qualified Literal
import qualified CoreSyn
import Var (TyVarBinder)
import TyCon (TyCon (..))
import Outputable

data DumpSimpl
  = DumpSimpl ModuleStat Decls

instance Outputable DumpSimpl where
  ppr (DumpSimpl ms decls) = ppr ms <+> ppr decls

type ModuleStat = String

-- We define @Type@ and some other things here, because they are not exported in ghc library.
data Decls = Decls [Bind Var]

instance Outputable Decls where
  ppr (Decls ds) = vcat $ map ppr ds

data Bind b
  = NonRec b (Func b)
  | Rec [(b, Func b)]

instance Outputable b => Outputable (Bind b) where
  ppr (NonRec b f) = text "nonrec" <> ppr b <> ppr f

data Func b
  = Func Type FuncStat (Expr b)

instance Outputable b => Outputable (Func b) where
  ppr (Func typ stat exp) = text "func" <+> ppr typ <+> ppr stat <+> ppr exp

type FuncStat = FastString
type KindOrType = Type

data Type
  = TyVarTy Var
  | AppTy Type Type
  | TyConApp TyCon [KindOrType]
  | ForallTy {-# UNPACK #-} !TyVarBinder Type
  | FunTy Type Type

instance Outputable Type where
  ppr (TyVarTy v) = ppr v
  ppr (AppTy t1 t2) = ppr t1 <> text " " <> ppr t2
  ppr (TyConApp con ts) = ppr con <> vcat (map ppr ts)
  ppr (ForallTy b t) = text "forall " <> ppr b <> text ". " <> ppr t
  ppr (FunTy t1 t2) = ppr t1 <> text " -> " <> ppr t2

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

instance Outputable b => Outputable (Expr b) where
  ppr (Var i) = ppr i
  ppr (Lit lit) = ppr lit
  ppr (App e1 as) = text "(" <> ppr e1 <> ppr as <> text ")"
  ppr (Lam b e) = text "λ" <> ppr b <> text ". " <> ppr e
  ppr (Let b e) = text "let " <> ppr b <> text " = { " <> ppr e <> text " }"
  ppr (Case e b t as) = text "case " <> ppr e <> text ", " <> ppr b <> text ", " <> ppr t <> text " of { " <> text "}"
  ppr (Tick t e) = text "tick {" <> ppr e <> text "}"
