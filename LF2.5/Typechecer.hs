module Typechecer where

import AbsLF
import PrintLF
import Prelude hiding (lookup)

data R a = OK a | Erro String
  deriving (Eq, Ord, Show, Read)

isError e = case e of
  OK _ -> False
  Erro _ -> True

type TContext = [(Ident, Type)]

typeCheckP :: Program -> [R TContext]
typeCheckP (Prog fs) =
  let nCtx = updatecF [] fs
   in case nCtx of
        OK ctx -> map (typeCheckF ctx) fs
        Erro msg -> [Erro msg]

typeCheckF :: TContext -> Function -> R TContext
typeCheckF tc (Fun tR _ decls exp) = tke (parameterTypeBindings ++ functionTypes) exp tR
  where
    parameterTypeBindings = map (\(Dec tp id) -> (id, tp)) decls
    functionTypes =
      filter
        ( \(i, t) -> case t of
            TFun _ _ -> True
            _ -> False
        )
        tc

{- "tke" é uma função que dado, um contexto de tipos, uma expressão, e um tipo,
   verifica se essa expressão tem esse tipo ou retorna um erro se a expressão-
   for mal tipada -}
tke :: TContext -> Exp -> Type -> R TContext
tke tc exp tp = do
  infType <- tinf tc exp
  if infType == tp
    then return tc
    else
      Erro
        ( "@typechecker: a expressao "
            ++ printTree exp
            ++ " tem tipo "
            ++ printTree infType
            ++ " mas o tipo esperado eh "
            ++ printTree tp
        )

{- "tinf" é uma função que dado, um contexto de tipos e uma expressão, retorna
   o tipo dessa expressão ou um erro se a expressão for mal tipada -}
tinf :: TContext -> Exp -> R Type
tinf tc x = case x of
  ECon exp0 exp -> combChecks tc exp0 exp TStr
  EAdd exp0 exp -> combChecks tc exp0 exp Tint
  ESub exp0 exp -> combChecks tc exp0 exp Tint
  EMul exp0 exp -> combChecks tc exp0 exp Tint
  EDiv exp0 exp -> combChecks tc exp0 exp Tint
  EOr exp0 exp -> combChecks tc exp0 exp Tbool
  EAnd exp0 exp -> combChecks tc exp0 exp Tbool
  ENot exp -> do
    _ <- tke tc exp Tbool
    return Tbool
  EStr _ -> return TStr
  ETrue -> return Tbool
  EFalse -> return Tbool
  EInt n -> return Tint
  EVar id -> lookup tc id
  EIf exp expT expE -> do
    _ <- tke tc exp Tint
    typeT <- tinf tc expT
    typeE <- tinf tc expE
    if typeT == typeE
      then return typeT
      else Erro "Expressoes no if nao batem"
  ECall id lexp -> checkCall tc id lexp

checkCall :: TContext -> Ident -> [Exp] -> R Type
checkCall tc id args = do
  funcType <- lookup tc id
  case funcType of
    TFun returnType paramTypes -> do
      if length paramTypes /= length args
        then Erro "LF2 nao tem lazy loading"
        else do
          let pairs = zip args paramTypes
          mapM_ (\(arg, expectedType) -> tke tc arg expectedType) pairs
          return returnType
    _ -> Erro "nao eh funcao"

combChecks :: TContext -> Exp -> Exp -> Type -> R Type
combChecks tc exp1 exp2 tp = do
  _ <- tke tc exp1 tp
  _ <- tke tc exp2 tp
  return tp

lookup :: TContext -> Ident -> R Type
lookup [] id = Erro ("@typechecker: " ++ printTree id ++ " nao esta no contexto. ")
lookup ((id, value) : cs) key
  | id == key = OK value
  | otherwise = lookup cs key

updateTC :: TContext -> Ident -> Type -> R TContext
updateTC [] id tp = return [(id, tp)]
updateTC ((id, tp) : idTps) idN tpN
  | id == idN = Erro ("@typechecker: identificador" ++ printTree id ++ " nao pode ter mais de um tipo")
  | otherwise = do
      rest <- updateTC idTps idN tpN
      return ((id, tp) : rest)

getFunctionType :: Function -> Type
getFunctionType (Fun tipoRetorno _ decls _) = TFun tipoRetorno (map (\(Dec tp _) -> tp) decls)

updatecF :: TContext -> [Function] -> R TContext
updatecF tc [] = OK tc
updatecF tc (f@(Fun _ nomeF _ _) : fs) =
  let r = updateTC tc nomeF (getFunctionType f)
   in case r of
        OK tcNew -> updatecF tcNew fs
        Erro msg -> Erro msg

instance Functor R where
  fmap f (OK x) = OK (f x)
  fmap _ (Erro s) = Erro s

instance Applicative R where
  pure = OK
  (OK f) <*> (OK x) = OK (f x)
  (Erro s) <*> _ = Erro s
  _ <*> (Erro s) = Erro s

instance Monad R where
  return = pure

  (OK x) >>= f = f x
  (Erro s) >>= _ = Erro s
