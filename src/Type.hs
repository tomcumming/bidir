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

asPoly :: Mono -> Poly
asPoly t = case t of
    MonoAtom t -> PolyAtom t
    MonoArrow t1 t2 -> PolyArrow (asPoly t1) (asPoly t2)

applySub :: Poly -> ExtId -> Poly -> Poly
applySub t x t2 = case t of
    PolyAtom (Ext y) | x == y -> t2
    PolyAtom _ -> t
    Forall y t -> Forall y (applySub t x t2)
    PolyArrow ta tr -> PolyArrow (applySub ta x t2) (applySub tr x t2)
