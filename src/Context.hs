module Context where

import qualified Type as Type

type ContextError = String

data Entry =
    TypeVar Type.Id
  | SolvedVar Type.Id Type.Poly
  | UnsolvedExt Type.ExtId
  | SolvedExt Type.ExtId Type.Mono
  | Marker Type.ExtId
  deriving (Eq, Show)

-- [b, a] in haskell but in the paper Γ,a,b
type Ctx = [Entry]

-- Γ[b][a] => [a][b]Γ = splitThree a b ctx
splitThree :: Entry -> Entry -> Ctx -> Maybe (Ctx, Ctx, Ctx)
splitThree a b ctx = do
    (ctx1, ctx2) <- splitTwo a ctx
    (ctx2, ctx3) <- splitTwo b ctx2
    return (ctx1, ctx2, ctx3)

-- Γ[a] => [a]Γ = splitThree a ctx
splitTwo :: Entry -> Ctx -> Maybe (Ctx, Ctx)
splitTwo a ctx = case ctx of
    (x:ctx) | x == a -> Just ([], ctx)
    (x:ctx) | Just (ctx1, ctx2) <- splitTwo a ctx -> Just (x:ctx1, ctx2)
    _ -> Nothing

validate :: Ctx -> Type.Poly -> Either ContextError ()
validate ctx t = case t of
    Type.PolyAtom t -> case t of
        Type.Unit -> return ()
        Type.Var x -> do
            maybe
                (Left "validate var")
                (\_ -> Right ())
                (splitTwo (TypeVar x) ctx)
