module RandomUtil where

import           System.Random
import           Control.Monad

rollDie :: Int -> IO Int
rollDie d = getStdRandom (randomR (1, d))

nTimesRoll :: Int -> Int -> IO [Int]
nTimesRoll n die = replicateM n (rollDie die)

