module Interpreter where

import AbsLF
import AbsLFAux -- TODO: leia agora o conteudo desse arquivo (AbsLFAux.hs) e explique por que refatoramos assim
-- Para facilitar manutencao ? Legibilidade ? Isolar responsabilidade ?
import Prelude hiding (lookup)

executeP :: Program -> Valor
executeP (Prog fs) = eval (updatecF [] fs) (expMain fs)
  where
    expMain (f : xs)
      | (getName f == (Ident "main")) = getExp f
      | otherwise = expMain xs

type RContext = [(Ident, Valor)]

eval :: RContext -> Exp -> Valor
eval context x = case x of
  ECon exp0 exp -> ValorStr (s (eval context exp0) ++ s (eval context exp))
  EAdd exp0 exp -> ValorInt (i (eval context exp0) + i (eval context exp))
  ESub exp0 exp -> ValorInt (i (eval context exp0) - i (eval context exp))
  EMul exp0 exp -> ValorInt (i (eval context exp0) * i (eval context exp))
  EDiv exp0 exp -> ValorInt (i (eval context exp0) `div` i (eval context exp))
  EOr exp0 exp -> ValorBool (b (eval context exp0) || b (eval context exp))
  EAnd exp0 exp -> ValorBool (b (eval context exp0) && b (eval context exp))
  ENot exp -> ValorBool (not (b (eval context exp)))
  EStr str -> ValorStr str
  ETrue -> ValorBool True
  EFalse -> ValorBool False
  EInt n -> ValorInt n
  EVar id -> lookup context id
  EIf exp expT expE ->
    if (i (eval context exp) /= 0)
      then eval context expT
      else eval context expE
  -- TODO: na linha abaixo, retorne um ValorFun contendo o lambda e saiba explicar a razao
  -- RESPOSTA: finaliza/compila a funcao em um valor
  lambda@(ELambda params exp) -> ValorFun lambda
  -- TODO: em EComp abaixo, troque undefined (2 ocorrencias) pela construcao apropriada
  EComp exp1 exp2 ->
    let (ValorFun exp1') = eval context exp1
        (ValorFun exp2') = eval context exp2
     in ValorFun
          -- acho estranho usar o lambda como "wrapper"
          -- ou seja tanto funcoes nomeadas quanto funcoes anonimas estao
          -- sobre o dominio de ELambda
          ( ELambda
              -- parametros de uma composicao sao os argumentos da "primeira" funcao
              -- exemplo: f(z) = z + 1 e g(x) = x + 2
              -- os parametros de f . g sao [x]
              (getParamsTypesL exp2')
              (ECall exp1' [ECall exp2' (getParamsExpL exp2')])
          )
  {- TODO: em ECall abaixo, troque undefined (3 ocorrencias) pela construcao apropriada.
     Dica: estude o codigo, buscando entender tambem as definicoes locais -}
  ECall exp argumentosExp ->
    if length argumentosExp < length parameters
      then ValorFun (ELambda params' exp') -- TODO: que caso eh esse ?
      -- RESPOSTA: Caso de aplicação parcial , nem todos os argumentos foram fornecidos.
      else eval (paramBindings ++ contextFunctions) exp' -- TODO: que caso eh esse ?
      -- RESPOSTA: Caso de aplicação total, todos os argumentos foram fornecidos.
    where
      -- errado ? so funciona por conta do typechecker ?
      (ValorFun lambda) = eval context exp
      -- parameters = apenas identificadores da funcao lambda
      parameters = getParamsL lambda
      -- liga o parametro ao seu valor "local"
      paramBindings = zip parameters (map (eval context) argumentosExp)
      -- parametros de lambda - quantidade de argumentos passados no call
      params' = drop (length argumentosExp) (getParamsTypesL lambda)
      -- substituindo os valores dos argumentos na expressao
      exp' = subst paramBindings (getExpL lambda)
      -- filtra o contexto para ter somente funcoes
      contextFunctions =
        filter
          ( \(i, v) -> case v of
              ValorFun _ -> True
              _ -> False
          )
          context

-- a função "subst" gera uma nova expressao a partir dos bindings em RContext
subst :: RContext -> Exp -> Exp
subst rc exp = case exp of
  EVar id -> bind id rc -- TODO: por que eh implementado assim ?
  -- RESPOSTA: É o caso base da substituição. A variável `id` é trocada pela expressão de seu valor, obtida do contexto `rc` através da função `bind`. Se não estiver no contexto, `bind` retorna a própria variável.
  -- TODO: explique a implementacao da linha abaixo
  -- RESPOSTA: Ao entrar em um novo escopo (lambda), removemos os parâmetros do lambda do contexto
  -- de substituição (`rc`) para evitar a captura acidental de variáveis com o mesmo nome de escopos externos (shadowing).
  lambda@(ELambda paramsTypes exp) -> ELambda paramsTypes (subst (rc `diff` (getParamsL lambda)) exp)
  ECall exp lexp -> ECall (subst rc exp) (map (subst rc) lexp)
  EAdd exp0 exp -> EAdd (subst rc exp0) (subst rc exp)
  -- TODO: nos casos abaixo, troque cada undefined pela construcao apropriada
  EComp exp1 exp2 -> EComp (subst rc exp1) (subst rc exp2)
  EIf expC expT expE -> EIf (subst rc expC) (subst rc expT) (subst rc expE)
  ECon exp0 exp -> ECon (subst rc exp0) (subst rc exp)
  ESub exp0 exp -> ESub (subst rc exp0) (subst rc exp)
  EMul exp0 exp -> EMul (subst rc exp0) (subst rc exp)
  EDiv exp0 exp -> EDiv (subst rc exp0) (subst rc exp)
  EOr exp0 exp -> EOr (subst rc exp0) (subst rc exp)
  EAnd exp0 exp -> EAnd (subst rc exp0) (subst rc exp)
  ENot exp -> ENot (subst rc exp)
  _ -> exp -- TODO: quais sao esses casos e por que sao implementados assim ?
  -- RESPOSTA: expressoes de constantes, a propria expressao ja eh o valor e portante nao necessita de substituicao

{- TODO:
  sobre a implementacao finalizada de subst:
  1) qual eh o caso base?
     R: O caso `_ -> exp` (literais) e `EVar id` (que termina a recursão para um ramo).
  2) como descrever o numero de casos recursivos? depende (in)diretamente de algo?
     R: O número de casos recursivos corresponde ao número de construtores de `Exp` que contêm subexpressões. Depende diretamente da definição da AST.
  3) qual a finalidade dos casos recursivos?
     R: Propagar a substituição por toda a árvore da expressão, garantindo que todas as subexpressões sejam processadas.
  4) por que a linha 64 eh diferente dos outros casos recursivos?
     R: Porque `ELambda` cria um novo escopo. É preciso modificar o contexto (`rc`) para remover os parâmetros do lambda, evitando a captura incorreta de variáveis (shadowing).
  5) numa especificacao textual intuitiva e concisa (semelhante ao comentario na linha 59),
     qual a linha mais importante entre 62-77 ?
     R: `EVar id -> bind id rc`, pois é onde a substituição da variável por seu valor efetivamente ocorre.
  6) Ha semelhanca de implementacao em relacao ao Optimizer.hs? Qual(is)?
     R: Sim. Ambas as funções (`subst` e `optimizeExp`) percorrem a AST recursivamente para reescrever a expressão, aplicando transformações específicas em cada nó.
-}

-- a função "diff" faz a diferença, tirando de RContext os mapeamentos envolvendo [Ident].
diff :: RContext -> [Ident] -> RContext
rc `diff` [] = rc
[] `diff` _ = []
((k, v) : kvs) `diff` (id : ids)
  | k == id = kvs `diff` ids
  | otherwise = (k, v) : (kvs `diff` (id : ids))

-- a função bind retorna uma expressao contendo o valor do id no RContext, ou o proprio id.
-- TODO: por que nao usamos o lookup no lugar de bind ?
-- REPOSTA: De fato muito parecidas, mas cada uma com um retorno diferente. Para o subst eh necessario uma expression e portanto deve ser usada a funcao bind
-- alem disso a logica para lidar com o valor nao encontrado no contexto nao faz sentido para a funcao lookup, elas tem tolerancia a erro diferentes ?
bind :: Ident -> RContext -> Exp
bind id [] = EVar id -- retorna o proprio id se ele nao esta ligado em RContext
bind id ((k, v) : kvs)
  | k == id = wrapValueExpression v
  | otherwise = bind id kvs

-- "wrapValueExpression" empacota um valor em uma expressao
wrapValueExpression :: Valor -> Exp
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse
wrapValueExpression (ValorFun exp) = exp

data Valor
  = ValorInt
      { i :: Integer
      }
  | ValorFun
      { f :: Exp -- f :: Function  **NOVO TODO: Por que mudou ?
      -- RESPOSTA: Mudou para Exp para que ValorFun possa representar não apenas funções nomeadas, mas também funções lambdas
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
  show (ValorFun f) = show f -- TODO: por que essa linha funciona ?
  -- RESPOSTA: pois ValoFun.f eh uma expressao e na definicao do tipo Exp temos o seguinte:
  --   deriving (C.Eq, C.Ord, C.Show, C.Read)

lookup :: RContext -> Ident -> Valor
lookup ((i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv

-- NOVO: TODO: explique a mudanca em updatecF
-- RESPOSTA: a mudanca acompanha a definicao de funcoes como valores ao tratar funcoes nomeadas tambem como funcoes lambda
-- isso "nivela" e consolida as funcoes como valores
updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f : fs) =
  updatecF
    ( update
        c
        (getName f)
        (ValorFun (ELambda (getParams f) (getExp f)))
    )
    fs

-- updatecF c (f:fs) = updatecF (update c (getName f) (ValorFun f)) fs
