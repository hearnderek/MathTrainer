module Equation where
import Operator
import Regimen
import EFA

-- Equation functions
data Equation = Equation (Operator, (Integer, Integer))

toEquation :: (Operator, (Integer, Integer)) -> Equation
toEquation a = (Equation a)

instance Show Equation where
  show e = printEquation e

printEquation :: Equation -> [Char]
printEquation (Equation (op, (a, b))) = (show a) ++ " " ++ (showOperator op) ++ " " ++ (show b)

eval :: Equation -> Integer
eval (Equation (op, (a,b))) = evalOperator op a b


evalMath :: Equation -> Integer -> Result
evalMath eq input = if input == answer 
                    then (True, "Correct!")
                    else (False, ("Wrong... " ++ (show answer)))
                    where answer = eval eq

{- IO

I struggled with trying to seperate out the IO from the pure functions.
Once one thing down the line needs a bit of IO suddenly your whole stack
needs it and writing the functions becomes just that much harder. I believe
I landed on a nice middle ground here. A little IO, some pure functions, a
little IO... etc.

-}
etraining :: [Equation] -> IO [Bool]
etraining [] = return []
etraining (eq:eqs) = do
  putStrLn $ printEquation eq
  input <- readLn :: IO Integer
  let (correct, outMsg) = evalMath eq input
  putStrLn outMsg

  remaining <- etraining eqs
  return (correct:remaining)


genEquations :: Regimen -> IO [Equation]
genEquations (R {times = t, maxNum = m, maxEnum = me}) = do
  xs     <- randos t m
  ys     <- randos t m
  opNums <- randos t me
  let numbers = zip xs ys
  let ops     = map numToOperator opNums
  return (map toEquation (zip ops numbers))

