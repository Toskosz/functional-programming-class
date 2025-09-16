module Tests where

import AbsLF

{-

double (x)
{
  x + x
}
quadruple (y)
{
  double (double (y))
}
main ()
{
  quadruple (3)
}

-}

test1 = Prog [Fun (Ident "double") [Ident "x"] (EAdd (EVar (Ident "x")) (EVar (Ident "x"))),Fun (Ident "quadruple") [Ident "y"] (ECall (Ident "double") [ECall (Ident "double") [EVar (Ident "y")]]),Fun (Ident "main") [] (ECall (Ident "quadruple") [EInt 3])]

-------------------------------------------------------

{-
sum (n)
{
  if (n) then n + sum (n - 1) else 0
}
main ()
{
  sum (10)
}

-}

test2 = Prog [Fun (Ident "sum") [Ident "n"] (EIf (EVar (Ident "n")) (EAdd (EVar (Ident "n")) (ECall (Ident "sum") [ESub (EVar (Ident "n")) (EInt 1)])) (EInt 0)),Fun (Ident "main") [] (ECall (Ident "sum") [EInt 10])]

----------------------------------------------------------

{-
main ()
{
  fat (5)
}
fat (n)
{
  if (n) then n * fat (n - 1) else 1
}

-}

test3 = Prog [Fun (Ident "main") [] (ECall (Ident "fat") [EInt 5]),Fun (Ident "fat") [Ident "n"] (EIf (EVar (Ident "n")) (EMul (EVar (Ident "n")) (ECall (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])) (EInt 1))]









