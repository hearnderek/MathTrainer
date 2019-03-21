import Equation
import Regimen
import Data.Time (getCurrentTime, diffUTCTime)
import BlankCount
import EFA
import System.Environment (getArgs)

{- Project Goals

1. Get me more comfortable with doing basic math,
   eliminating the gut reation of getting a calculator.
2. Give me some more experience with Haskell's type system.
3. Use multiple files.
4. Have easy to reason about command line arguemnts.

-}

numCorrect :: [Bool] -> [Char]
numCorrect xs = ("----" ++ (show correct) ++ " out of " ++ (show total) ++ "----")
  where correct = length (filter true xs)
        total   = length xs
        true b  = b

train :: Regimen -> IO()
train regimen = do
  equations <- genEquations regimen
  start     <- getCurrentTime
  results   <- etraining equations
  end       <- getCurrentTime
  putStrLn $ numCorrect results
  putStrLn ("Completed in " ++ (show (diffUTCTime end start)))

trainB :: BlankRegimen -> IO()
trainB regimen = do
  start   <- getCurrentTime
  results <- bTraining regimen
  end     <- getCurrentTime
  putStrLn $ numCorrect results
  putStrLn ("Completed in " ++ (show (diffUTCTime end start)))

needsHelp :: [String] -> Bool
needsHelp xs = any (=="--help") xs

run :: [String] -> IO ()
run args = do
    let rs = fromArgs args :: [Regimen]
    let bs = fromArgs args :: [BlankRegimen]
    mapM_ Main.trainB bs
    mapM_ Main.train rs

-- Entry Point
main :: IO ()
main = do
  putStrLn "Welcome to TRAINER.hs"
  args <- getArgs
  if (needsHelp args) 
  then putStrLn help
  else run args
  putStrLn "End"

-- Multi-line strings didn't work as I expect them to...
help :: [Char]
help = "the Arguments for TRAINER.hs are [-r <num-questions> <max-number> <1..4>]\n\
\the <1..4> represent the max operator to use\n\
\1 = +\n\
\2 = -\n\
\3 = *\n\
\4 = /\n\
\\n\
\if you give no arguments it just runs a defualt training regimen\n\
\to run the default it would look like\n\
\  trainer.exe -r 10 10 4 -r 10 50 2"
