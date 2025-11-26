module Interpreter where

import AbsLF
-- import Tests
import Prelude hiding (lookup)

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
  EVar id -> (lookup context id, context)
  {- dica: estude a semântica do "SIf" na LI2 e saiba explicar a diferença -}
  EIf exp expT expE ->
    let (v0, ctx0) = eval context exp
     in if i v0 /= 0
          then eval ctx0 expT
          else eval ctx0 expE
  {- @dica: não altere o resto, mas saiba explicar o funcionamento -}
  ECall id lexp -> case lookup context id of
    (ValorFun funDef) ->
      let parameters = map (\(Dec _ ident) -> ident) (getParams funDef)
          onlyValues = map (\(value, ctx) -> value) (map (eval context) lexp)
          paramBindings = zip parameters onlyValues
          contextFunctions =
            filter
              ( \(i, v) -> case v of
                  ValorFun _ -> True
                  _ -> False
              )
              context
          (res, ignoreCtx) = eval (paramBindings ++ contextFunctions) (getExp funDef)
       in (res, context)
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

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f

-- (\(Ident x) -> x) nf

type RContext = [(Ident, Valor)]

lookup :: RContext -> Ident -> Valor
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv

updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f : fs) = updatecF (update c (getName f) (ValorFun f)) fs
