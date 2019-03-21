import Equation
import BlankCount
import EFA
import Input
import Control.Monad (replicateM)

testBlank :: IO()
testBlank = do
  let b = Blank 5
  print b
  let guess = 4
  print 4
  let result = EFA.train b 4
  print result

testBlankRegimen :: IO()
testBlankRegimen = do
  let args = ["-a","5","5"]
  print args
  let input = repeat "1"
  let bs = fromArgs args :: [BlankRegimen]
  print bs
  let qs = map (flip questions [5..10]) bs
  print qs
  let guesses = [5..]
  let results gs = map (\xs -> zipWith EFA.train xs gs) qs
  print $ results guesses
  let badGuesses = [1..]
  print $ results badGuesses

testInput = do
  let inio = allInputi
  let neededIn = take 5 inio
  truIn <- takeIn 5
  print $ head truIn
  takeIn 5 >>= print
  
testAnswer = do
  let b = Blank 5
  print b
  num <- nextGoodIntQues "How many '*'s?" "That's not a number..."
  print (train b num)

testMain :: IO ()
testMain = do
  let args = ["-a","10","10"]
  let bs = fromArgs args :: [BlankRegimen]
  results <- sequence $ map runStuff bs
  print results
  
