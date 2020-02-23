module RandomUtil where

import           System.Random
import           Control.Monad

-- | Generate random integer between given range (inclusive)
randInt :: (Int, Int) -> IO Int
randInt (low, high) = getStdRandom (randomR (low, high))

-- | Roll a d-sided die
rollDie :: Int -> IO Int
rollDie d = randInt (1, d)

-- | Roll n times a d-sided die and return a list of the results
--
-- >>> nTimesRoll 3 6
-- [2, 6, 3]
nTimesRoll :: Int -> Int -> IO [Int]
nTimesRoll n d = replicateM n (rollDie d)
