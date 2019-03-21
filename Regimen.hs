module Regimen where
import EFA

{- Data Types & Record Syntax

This Reginmen data type, which uses Record Syntax to declare the members,
exists to better define what the arguments are for my train, and 
genEquation functions. I think we can all agree that bare numbers are not
the best vessel for easy to read arguments.

Of course while running thought this I hit the dark underbellow of haskell:
Record Clashing... I couldn't name maxNum as max as I tried initially due
to another record existing with that name being used by one of its members.

-}
data Regimen = R { times, maxNum, maxEnum :: Integer } deriving Show

instance EFA Regimen where
  fromArgs args = genEFA args processArgs defaultRegimens

defaultRegimens :: [Regimen]
defaultRegimens = (R {times = 10, maxNum = 10, maxEnum = 4}) : (R {times = 10, maxNum = 50, maxEnum = 2}) : []

{-

The hardest question is what arguments do I want, rather than how do I read them.

-r for each regimen
then number for number of question
then number for max number used
then number 1..4 for operator enum
*.exe -r 100 50 1 -r 10 50 2 -a 50 100
-}

-- This just ignores bad input.
processArgs :: [[Char]] -> [Regimen]
processArgs ("-r":timesStr:numStr:numEnum:ras) = 
  (R{
    times   = (read timesStr :: Integer), 
    maxNum  = (read numStr   :: Integer), 
    maxEnum = (read numEnum  :: Integer)
  }) : processArgs ras
processArgs _ = []

