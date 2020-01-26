{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Character.Attributes where

import           RandomUtil                     ( nTimesRoll )
import           Control.Monad                  ( replicateM )
import           Data.Maybe                     ( fromJust )
import           Data.Aeson
import           GHC.Generics
import           Data.List

data Attributes =
    Attributes { str :: Int, dex :: Int, con :: Int
               , int :: Int, wis :: Int, cha :: Int}
    deriving (Show, Read, Eq, Generic)

instance ToJSON Attributes
instance FromJSON Attributes

fromList :: [Int] -> Maybe Attributes
fromList xs
    | length xs == 6
    = let [str, dex, con, int, wis, cha] = xs
      in  Just (Attributes str dex con int wis cha)
    | otherwise
    = Nothing


randomAttributes3D6 :: IO Attributes
randomAttributes3D6 = do
    attrs <- replicateM 6 (nTimesRoll 3 6)
    (return . fromJust . fromList . map sum) attrs

randomAttributes4D6BestOf3 :: IO Attributes
randomAttributes4D6BestOf3 = do
    attrs <- replicateM 6 (nTimesRoll 4 6)
    return
        $ (fromJust . fromList . map sum . map
              (take 3 . sortBy (flip $ compare))
          )
              attrs
