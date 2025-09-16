module Main where

import AbsLF
import ErrM
import Interpreter
import LexLF
import ParLF

main = do
  interact calc
  putStrLn ""

calc s = case (pProgram (myLexer s)) of
  Ok p -> show (executeP p)
  Bad s -> show s

{-
  let Ok p = pProgram  (myLexer s)
  in show (executeP p)
-}
