module RandomUtil where

import           System.Random
import           Control.Monad
import           Data.Set

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

-- | Choose random member of non-empty list
choose :: [a] -> IO a
choose xs = do
    let l = length xs - 1
    randomIndex <- randInt (0, l)
    return $ xs !! randomIndex

-- | Choose random member of non-empty set
chooseFromSet :: Set a -> IO a
chooseFromSet s = choose (elems s)
