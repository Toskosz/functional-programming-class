-- https://gist.github.com/eusuke/3833162

module Optimizer where

import AbsLF
import Data.Generics
import Interpreter

optimizeP :: Program -> Program
optimizeP = everywhere (mkT optimize)

optimize :: Exp -> Exp
optimize exp = case exp of
  EStr str -> EStr str
  ETrue -> ETrue
  EFalse -> EFalse
  EInt n -> EInt n
  EVar id -> EVar id
  ENot e1 -> foldIfConstant exp [e1]
  ECon exp0 exp1 -> foldIfConstant exp [exp0, exp1]
  EAdd exp0 exp1 -> foldIfConstant exp [exp0, exp1]
  ESub exp0 exp1 -> foldIfConstant exp [exp0, exp1]
  EMul exp0 exp1 -> foldIfConstant exp [exp0, exp1]
  EDiv exp0 exp1 -> foldIfConstant exp [exp0, exp1]
  EOr exp0 exp1 -> foldIfConstant exp [exp0, exp1]
  EAnd exp0 exp1 -> foldIfConstant exp [exp0, exp1]
  EIf (EInt v) expT expE -> if v /= 0 then expT else expE
  _ -> exp

foldIfConstant :: Exp -> [Exp] -> Exp
foldIfConstant originalExp children
  | all isLiteral children =
      let (v, _) = eval [] originalExp
       in wrapValueExpression v
  | otherwise = originalExp

isLiteral :: Exp -> Bool
isLiteral exp = case exp of
  EStr _ -> True
  ETrue -> True
  EFalse -> True
  EInt _ -> True
  _ -> False

wrapValueExpression :: Valor -> Exp
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse
