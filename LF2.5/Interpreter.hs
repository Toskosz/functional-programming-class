module Interpreter where

import AbsLF
-- import Tests
import Prelude hiding (lookup)
import qualified Prelude as C (Eq, Ord, Read, Show)

type Interpreter a = State RContext a

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
  let (returnValue, newContext) = runState (eval (expMain fs)) (updatecF [] fs)
   in returnValue
  where
    expMain (f : xs)
      | getName f == Ident "main" = getExp f
      | otherwise = expMain xs

eval :: Exp -> Interpreter Valor
eval x = case x of
  ECon exp0 exp -> do
    v0 <- eval exp0
    v1 <- eval exp
    return $ ValorStr (s v0 ++ s v1)
  EAdd exp0 exp -> do
    v0 <- eval exp0
    v1 <- eval exp
    return $ ValorInt (i v0 + i v1)
  ESub exp0 exp -> do
    v0 <- eval exp0
    v1 <- eval exp
    return $ ValorInt (i v0 - i v1)
  EMul exp0 exp -> do
    v0 <- eval exp0
    v1 <- eval exp
    return $ ValorInt (i v0 * i v1)
  EDiv exp0 exp -> do
    v0 <- eval exp0
    v1 <- eval exp
    return $ ValorInt (i v0 `div` i v1)
  EOr exp0 exp -> do
    v0 <- eval exp0
    v1 <- eval exp
    return $ ValorBool (b v0 || b v1)
  EAnd exp0 exp -> do
    v0 <- eval exp0
    v1 <- eval exp
    return $ ValorBool (b v0 && b v1)
  ENot exp -> do
    v <- eval exp
    return $ ValorBool (not (b v))
  EStr str -> return $ ValorStr str
  ETrue -> return $ ValorBool True
  EFalse -> return $ ValorBool False
  EInt n -> return $ ValorInt n
  EVar id -> do
    context <- get
    case safeLookup context (VarId id) of
      Just val -> return val
      Nothing -> error ("Variavel nao existe")
  EIf exp expT expE -> do
    v0 <- eval exp
    if i v0 /= 0
      then eval expT
      else eval expE
  ECall id lexp -> do
    argValues <- mapM eval lexp
    let cacheKey = MemoId id argValues
    context <- get
    case safeLookup context cacheKey of
      Just cachedVal -> return cachedVal
      Nothing -> case safeLookup context (VarId id) of
        Just (ValorFun funDef) -> do
          let parameters = map (\(Dec _ ident) -> ident) (getParams funDef)
          let paramBindings = zip parameters argValues
          let paramEntries = map (\(p, v) -> (VarId p, v)) paramBindings

          modify (paramEntries ++)

          res <- eval (getExp funDef)

          modify
            ( \ctx ->
                let ctxWithCache = update ctx cacheKey res
                 in removeParams ctxWithCache parameters
            )

          return res
        _ -> error "Erro chamando funcao"

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State g) = State $ \s ->
    let (x, s') = g s
     in (f x, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  (State f) <*> (State g) = State $ \s ->
    let (func, s') = f s
        (val, s'') = g s'
     in (func val, s'')

instance Monad (State s) where
  return = pure

  (State h) >>= f = State $ \s ->
    let (val, newState) = h s
        (State g) = f val
     in g newState

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

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
