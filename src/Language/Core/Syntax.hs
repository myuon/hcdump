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
data Decls = Decls [Bind]
  deriving (Eq, Show)

data Bind
  = NonRec Var Func
  | Rec [(Var, Func)]
  deriving (Eq, Show)

instance Ppr Bind where
  ppr (NonRec id (Func typ info exp))
    = green (ppr id) <+> red (string "::") <+> ppr typ
    <$$> ppr info
    <$$> ppr id <+> red (string "=") <+> ppr exp

newtype IdInfo = IdInfo { getIdInfo :: [(FastString, FastString)] }
  deriving (Eq, Show)

instance Ppr IdInfo where
  ppr (IdInfo xs)
    = red (string "[")
    <> hcat (intersperse (red (string ", ")) $ map (\(x,y) ->
      string (FS.unpackFS x)
      <+> red (string "=")
      <+> string (FS.unpackFS y)) xs)
    <> red (string "]")

data Func
  = Func Type IdInfo Expr
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
  ppr (TyConApp t xs) = ppr t <> hcat (map ppr xs)

data Var
  = Token FastString
  | QToken FastString FastString
  deriving (Eq, Show)

instance Ppr Var where
  ppr (Token s) = string $ FS.unpackFS s
  ppr (QToken q s) = dullmagenta (string (FS.unpackFS q)) <> "." <> string (FS.unpackFS s)

type Id = Var
type Alt b = ()
type Tickish b = ()

data Literal
  = LitNumber Integer Bool
  | MachStr B.ByteString Bool
  deriving (Eq, Show)

instance Ppr Literal where
  ppr (LitNumber n b) = blue $ integer n <> (if b then "#" else "")
  ppr (MachStr bs b) = yellow $ string (show bs) <> (if b then "#" else "")

data Expr
  = Var Id
  | Lit Literal
  | App Expr Expr
  | Lam [(Var, Type)] Expr
  | Let Bind Expr
  | Case Expr Var Type [Alt Var]
  | Cast Expr Coercion Type
  | Tick (Tickish Id) Expr
  | Type Type
  deriving (Eq, Show)

instance Ppr Expr where
  ppr (Var x) = ppr x
  ppr (Lit lit) = ppr lit
  ppr (App e1 e2) = "(" <> ppr e1 <> ")" <+> "(" <> ppr e2 <> ")"
  ppr (Type t) = "@" <+> ppr t
  ppr (Cast e _ _) = ppr e <+> blue "`cast`" <+> "(..)"

data Role = Nominal | Representational | Phantom
  deriving (Eq, Show)

roleChar :: Role -> Char
roleChar Nominal          = 'N'
roleChar Representational = 'R'
roleChar Phantom          = 'P'

type CoercionN = Coercion
data Coercion
  = Refl Role Type
  | SymCo Coercion
  | NthCo Role Int Coercion
  | TyConAppCo Role TyCon [Coercion]
  | AppCo Coercion CoercionN
  deriving (Eq, Show)
