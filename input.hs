module Input where

readMaybe :: String -> Maybe Integer
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

allGoodInput :: [Maybe t] -> [t]
allGoodInput ((Just a):xs) = a : (allGoodInput xs)
allGoodInput (Nothing:xs) = allGoodInput xs
allGoodInput [] = []

tillGoodInput :: [Maybe t] -> t
tillGoodInput ((Just a):_) = a
tillGoodInput (Nothing:xs) = tillGoodInput xs
tillGoodInput [] = error "Empty List"

nextGoodIntQues :: String -> String -> IO Integer
nextGoodIntQues question badInMessage = do
  putStrLn question
  nextGoodInput badInMessage

-- keeps asking for input until it is a number
-- The string is the message to display on invalid input.
nextGoodInput :: String -> IO Integer
nextGoodInput badInMessage = do
  ln <- getLine
  let ini = readMaybe ln
  case ini of
    Nothing -> do
      putStrLn badInMessage
      nextGoodInput badInMessage
    Just x -> return x

nextGoodInputStdMsg :: IO Integer
nextGoodInputStdMsg = nextGoodInput "That's not a number. Please enter a number."

getAllInput :: IO [Maybe Integer]
getAllInput = do
  ln <- getLine 
  let m = (readMaybe ln :: (Maybe Integer))
  remainder <- getAllInput
  return (m:remainder)
 
-- Asks for all the inputs in a row.
takeIn :: Int -> IO [String]
takeIn n = sequence $ take n allInputi

--Infinite
allInput :: IO [String]
allInput = do
  ln <-getLine 
  remainder <- allInput
  return (ln : remainder)

allInputi :: [IO String]
allInputi = getLine : allInputi
