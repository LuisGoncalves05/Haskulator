{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where

import Parsing ( Parser, parse, (<|>), satisfy, char, many1 )
import Data.Char
import Data.List

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
data Command = Assign Name Expr
             | Evaluate Expr

data Expr = Num Integer
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Rem Expr Expr
          deriving Show

type Name = String
type Env = [(Name, Integer)]

-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> Integer
eval e (Num n) = n
eval e (Var e1) = case find (\x -> fst x == e1) e of
  Just result -> snd result
  Nothing -> error "undefined variable"
eval e (Add e1 e2) = eval e e1 + eval e e2
eval e (Mul e1 e2) = eval e e1 * eval e e2
eval e (Sub e1 e2) = eval e e1 - eval e e2
eval e (Div e1 e2) = eval e e1 `div` eval e e2
eval e (Rem e1 e2) = eval e e1 `rem` eval e e2

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|> do char '-'
                      t <- term
                      exprCont (Sub acc t)
               <|> return acc

term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor
                   termCont (Mul acc f)
                 <|> do char '/'
                        f <- factor
                        termCont (Div acc f)
                 <|> do char '%'
                        f <- factor
                        termCont (Rem acc f)
                 <|> return acc

factor :: Parser Expr
factor =  do v <- variable
             return (Var v)
         <|>
          do n <- natural
             return (Num n)
         <|>
          do char '('
             e <- expr
             char ')'
             return e


natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser Name
variable = do many1 (satisfy isAlpha)

command :: Parser Command
command = do v <- variable
             char '='
             e <- expr
             return (Assign v e)
          <|> do e <- expr
                 return (Evaluate e)

----------------------------------------------------------------             

main :: IO ()
main = do txt <- getContents
          calculator [] (lines txt)

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator _ []  = return ()
calculator e (l:ls) = do let (str, newe) = execute e l
                         putStrLn str
                         calculator newe ls

-- | evaluate a single expression
execute :: Env -> String -> (String, Env)
execute env txt = case parse command txt of
  [(Assign v e, "")] -> (show val, newe)
                        where 
                          val = eval env e
                          newe = (v, val) : env
  [(Evaluate e, "")] -> (show (eval env e), env)
  _ -> ("parse error; try again", env)