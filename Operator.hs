module Operator where

{-

My first attempt at using types.

-}

type Operator = Char
numToOperator :: Integer -> Operator
numToOperator 1 = '+'
numToOperator 2 = '-'
numToOperator 3 = '*'
numToOperator 4 = '/'
numToOperator _ = error "Bad Operator Enum"

showOperator :: Operator -> [Char]
showOperator op = [op]

evalOperator :: Operator -> (Integer -> Integer -> Integer)
evalOperator '+' = (+)
evalOperator '-' = (-)
evalOperator '*' = (*)
evalOperator '/' = (div)
evalOperator _ = error "Bad Operator Enum"
