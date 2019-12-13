module Context where

import Type (Id, ExtId, Mono, Poly)

data Entry =
    TypeVar Id
  | SolvedVar Id Poly
  | UnsolvedExt ExtId
  | SolvedExt ExtId Mono
  | Marker ExtId
  deriving (Eq, Show)

-- [b, a] in haskell but in the paper Γ,a,b
type Ctx = [Entry]

-- Γ[b][a] => [a][b]Γ = splitThree a b ctx
splitThree :: ExtId -> ExtId -> Ctx -> Maybe (Ctx, Ctx, Ctx)
splitThree a b ctx = do
    (ctx1, ctx2) <- splitTwo a ctx
    (ctx2, ctx3) <- splitTwo b ctx2
    return (ctx1, ctx2, ctx3)

-- Γ[a] => [a]Γ = splitThree a ctx
splitTwo :: ExtId -> Ctx -> Maybe (Ctx, Ctx)
splitTwo a ctx = case ctx of
    (x:ctx) | x == UnsolvedExt a -> Just ([], ctx)
    (x:ctx) | Just (ctx1, ctx2) <- splitTwo a ctx -> Just (x:ctx1, ctx2)
    _ -> Nothing
