module Context where

import qualified Type as Type
import qualified Expr as Expr

type ContextError = String

data Entry =
    TypeVar Type.Id
  | SolvedVar Expr.Id Type.Poly
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
        Type.Ext x -> do
            let se = hasSolvedExt x ctx
            case (hasSolvedExt x ctx, splitTwo (UnsolvedExt x) ctx ) of
                (True, _) -> Right ()
                (_, Just _) -> Right ()
                _ -> Left "validate ext"
    Type.Forall x t -> validate (TypeVar x:ctx) t
    Type.PolyArrow t1 t2 -> do
        validate ctx t1
        validate ctx t2

hasSolvedExt :: Type.ExtId -> Ctx -> Bool
hasSolvedExt x ctx = case ctx of
    (SolvedExt y _:_) | x == y -> True
    (_:ctx) -> hasSolvedExt x ctx
    [] -> False

apply :: Ctx -> Type.Poly -> Type.Poly
apply ctx t = case ctx of
    (SolvedExt x t2:ctx) -> apply ctx (Type.applySub t x (Type.asPoly t2))
    (_:ctx) -> apply ctx t
    [] -> t

lookup :: Ctx -> Expr.Id -> Maybe Type.Poly
lookup ctx x = case ctx of
    (SolvedVar y t:_) | x == y -> Just t
    (_:ctx) -> Context.lookup ctx x
    [] -> Nothing
