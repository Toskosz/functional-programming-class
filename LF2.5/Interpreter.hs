module Interpreter where

import AbsLF
-- import Tests
import Prelude hiding (lookup)
import qualified Prelude as C (Eq, Ord, Read, Show)

getType :: Function -> Type
getType (Fun t _ _ _) = t

getName :: Function -> Ident
getName (Fun _ name _ _) = name

getParams :: Function -> [Decl]
getParams (Fun _ _ params _) = params

getExp :: Function -> Exp
getExp (Fun _ _ _ expression) = expression

executeP :: Program -> Valor
executeP (Prog fs) =
  let (returnValue, newContext) = eval (updatecF [] fs) (expMain fs)
   in returnValue
  where
    expMain (f : xs)
      | getName f == Ident "main" = getExp f
      | otherwise = expMain xs

eval :: RContext -> Exp -> (Valor, RContext)
eval context x = case x of
  ECon exp0 exp ->
    let (v0, ctx0) = eval context exp0
        (v1, ctx1) = eval ctx0 exp
     in (ValorStr (s v0 ++ s v1), ctx1)
  EAdd exp0 exp ->
    let (v0, ctx0) = eval context exp0
        (v1, ctx1) = eval ctx0 exp
     in (ValorInt (i v0 + i v1), ctx1)
  ESub exp0 exp ->
    let (v0, ctx0) = eval context exp0
        (v1, ctx1) = eval ctx0 exp
     in (ValorInt (i v0 - i v1), ctx1)
  EMul exp0 exp ->
    let (v0, ctx0) = eval context exp0
        (v1, ctx1) = eval ctx0 exp
     in (ValorInt (i v0 * i v1), ctx1)
  EDiv exp0 exp ->
    let (v0, ctx0) = eval context exp0
        (v1, ctx1) = eval ctx0 exp
     in (ValorInt (i v0 `div` i v1), ctx1)
  EOr exp0 exp ->
    let (v0, ctx0) = eval context exp0
        (v1, ctx1) = eval ctx0 exp
     in (ValorBool (b v0 || b v1), ctx1)
  EAnd exp0 exp ->
    let (v0, ctx0) = eval context exp0
        (v1, ctx1) = eval ctx0 exp
     in (ValorBool (b v0 && b v1), ctx1)
  ENot exp -> let (v, ctx) = eval context exp in (ValorBool (not (b v)), ctx)
  EStr str -> (ValorStr str, context)
  ETrue -> (ValorBool True, context)
  EFalse -> (ValorBool False, context)
  EInt n -> (ValorInt n, context)
  EVar id -> (lookup context (VarId id), context)
  {- dica: estude a semântica do "SIf" na LI2 e saiba explicar a diferença -}
  EIf exp expT expE ->
    let (v0, ctx0) = eval context exp
     in if i v0 /= 0
          then eval ctx0 expT
          else eval ctx0 expE
  {- @dica: não altere o resto, mas saiba explicar o funcionamento -}
  ECall id lexp ->
    let onlyValues = map (\(value, ctx) -> value) (map (eval context) lexp)
        cacheKey = MemoId id onlyValues
     in case safeLookup context cacheKey of
          Just cachedVal -> (cachedVal, context)
          Nothing -> case lookup context (VarId id) of
            (ValorFun funDef) ->
              let parameters = map (\(Dec _ ident) -> ident) (getParams funDef)
                  paramBindings = zip parameters onlyValues

                  paramEntries = map (\(p, v) -> (VarId p, v)) paramBindings
                  executionEnvironment = paramEntries ++ context

                  (res, ctxAfter) = eval executionEnvironment (getExp funDef)

                  ctxWithCache = update ctxAfter cacheKey res

                  finalCtx = removeParams ctxWithCache parameters
               in (res, finalCtx)
            value -> (value, context)

-- *** @dica: nao altere o todo o codigo abaixo a partir daqui

{-
data Valor = ValorInt Integer |
             ValorStr String
i (ValorInt vi) = vi
s (ValorStr vs) = vs
-}

data Valor
  = ValorInt
      { i :: Integer
      }
  | ValorFun
      { f :: Function
      }
  | ValorStr
      { s :: String
      }
  | ValorBool
      { b :: Bool
      }
  deriving (C.Eq)

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f

data ContextIdent
  = VarId Ident
  | MemoId Ident [Valor]
  deriving (C.Eq)

-- (\(Ident x) -> x) nf

type RContext = [(ContextIdent, Valor)]

lookup :: RContext -> ContextIdent -> Valor
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

safeLookup :: RContext -> ContextIdent -> Maybe Valor
safeLookup [] _ = Nothing
safeLookup ((id, v) : cs) searchId
  | id == searchId = Just v
  | otherwise = safeLookup cs searchId

update :: RContext -> ContextIdent -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv

updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f : fs) = updatecF (update c (VarId (getName f)) (ValorFun f)) fs

removeParams :: RContext -> [Ident] -> RContext
removeParams ctx [] = ctx
removeParams ((VarId id, v) : cs) (p : ps)
  | id == p = removeParams cs ps
  | otherwise = (VarId id, v) : removeParams cs (p : ps)
removeParams ((MemoId id args, v) : cs) (p : ps) = removeParams cs (p : ps)
