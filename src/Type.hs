module Type where

type Id = String
type ExtId = Int

data Atom =
    Unit
  | Var Id
  | Ext ExtId
  deriving (Show, Eq)

data Mono =
    MonoAtom Atom
  | MonoArrow Mono Mono
  deriving (Show, Eq)

data Poly =
    PolyAtom Atom
  | Forall Id Poly
  | PolyArrow Poly Poly
  deriving (Show, Eq)
