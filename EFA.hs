module EFA where
import System.Random (randomRIO)
import Control.Monad (replicateM)

data EFAResult = EFAResult [Result]

instance Show EFAResult where
  show (EFAResult rs)  = (show numTrue) ++ " out of " ++ (show total) 
    where identity a = a
          numTrue    = length (filter identity bools)
          total      = length bools
          bools      = map fst rs

type Result = (Bool, [Char])

class EFA a where
  fromArgs :: [String] -> [a]

genEFA :: [String] -> ([String] -> [a]) -> [a] -> [a]
genEFA args processArgs defaults = 
  if (null args) then defaults else processArgs args

class EFAQ a where
  train :: a -> Integer -> Result

genMessage :: (Eq a, Show a) => a -> a -> String
genMessage actual guess = 
  if actual == guess 
  then "Correct! "
  else "Incorrect... " ++ (show actual)

randos :: Integer -> Integer -> IO [Integer]
randos n max = replicateM (fromIntegral n) (randomRIO (1, max) :: IO Integer)

