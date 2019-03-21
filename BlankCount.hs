module BlankCount where
import EFA
import Input

data BlankRegimen = BlankRegimen Integer Integer deriving Show

defaultBlanks :: [BlankRegimen]
defaultBlanks = [BlankRegimen 10 10]

data Blank = Blank Integer

instance EFAQ Blank where
  train (Blank b) a = (correct, message)
    where correct   = b == a
          message   = genMessage b a

instance Show Blank where
  show (Blank b) = replicate (fromIntegral b) '*'

instance EFA BlankRegimen where
  fromArgs args = blanksEFA args

questions :: BlankRegimen -> [Integer] -> [Blank]
questions r (num:numbers) = (Blank num) : (questions r numbers)
questions _ [] = []

blanksEFA :: [String] -> [BlankRegimen]
blanksEFA args = genEFA args processArgs defaultBlanks

processArgs :: [String] -> [BlankRegimen]
processArgs ("-a":k:m:xs) = (BlankRegimen (read k :: Integer) (read m :: Integer)) : (processArgs xs)
processArgs _ = []

bTraining :: BlankRegimen -> IO [Bool]
bTraining (BlankRegimen x y) = do
  rs <- randos x y
  let blanks = map Blank rs
  sequence $ map bTrain blanks

bTrain :: Blank -> IO (Bool)
bTrain b = do
  print b
  num <- nextGoodInputStdMsg
  let (isCorrect, message) = train b num
  putStrLn message
  return isCorrect
