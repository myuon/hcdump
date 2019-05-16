module Language.Core.Syntax where

import Prelude hiding ((<>))
import qualified Data.ByteString as B
import Data.List
import FastString (FastString)
import qualified FastString as FS
import Text.PrettyPrint.ANSI.Leijen

class Ppr p where
  ppr :: p -> Doc

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

instance Ppr (Bind Var) where
  ppr (NonRec id (Func typ info exp))
    = green (ppr id) <+> red (string "::") <+> ppr typ
    <$$> ppr info
    <$$> ppr id <+> red (string "=") <+> ppr exp

newtype IdInfo = IdInfo { getIdInfo :: [(FastString, FastString)] }
  deriving (Eq, Show)

instance Ppr IdInfo where
  ppr (IdInfo xs)
    = red (string "[")
    <> hcat (intersperse (red (string ",")) $ map (\(x,y) ->
      string (FS.unpackFS x)
      <+> red (string "=")
      <+> string (FS.unpackFS y)) xs)
    <> red (string "]")

data Func b
  = Func Type IdInfo (Expr b)
  deriving (Eq, Show)

type KindOrType = Type
type TyVarBinder = ()
type TyCon = Var

data Type
  = TyVarTy Var
  | AppTy Type Type
  | TyConApp TyCon [KindOrType]
  | ForallTy TyVarBinder Type
  | FunTy Type Type
  deriving (Eq, Show)

instance Ppr Type where
  ppr (TyVarTy v) = ppr v
  ppr (AppTy t1 t2) = ppr t1 <+> ppr t2
  ppr (TyConApp t xs) = ppr t <+> hcat (map ppr xs)

data Var
  = Token FastString
  | QToken FastString FastString
  deriving (Eq, Show)

instance Ppr Var where
  ppr (Token s) = string $ FS.unpackFS s
  ppr (QToken q s) = dullmagenta (string (FS.unpackFS q)) <> "." <> string (FS.unpackFS s)

type Id = Var
type Coercion = Type
type Arg b = Expr b
type Alt b = ()
type Tickish b = ()

data Literal
  = LitNumber Integer
  | MachStr B.ByteString
  deriving (Eq, Show)

instance Ppr Literal where
  ppr (LitNumber n) = blue $ integer n
  ppr (MachStr bs) = yellow $ string $ show bs

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

instance Ppr (Expr Var) where
  ppr (Var x) = ppr x
  ppr (Lit lit) = ppr lit
  ppr (App e1 e2) = ppr e1 <+> ppr e2
