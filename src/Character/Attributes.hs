{-# LANGUAGE DeriveGeneric #-}
module Character.Attributes where

import           RandomUtil                     ( nTimesRoll )
import           Control.Monad                  ( replicateM )
import           Data.Maybe                     ( fromJust )
import           Data.Aeson
import           GHC.Generics
import           Database.SQLite.Simple.FromRow
import           Data.List

-- | Attributes represent the characters physical and mental qualities
data Attributes =
    Attributes { str :: Int -- ^ Strength
               , dex :: Int -- ^ Dexterity
               , con :: Int -- ^ Constitution
               , int :: Int -- ^ Intelligence
               , wis :: Int -- ^ Wisdom
               , cha :: Int -- ^ Charisma
               }
    deriving (Show, Read, Eq, Generic)

instance ToJSON Attributes
instance FromJSON Attributes
instance FromRow Attributes where
    fromRow =
        Attributes <$> field <*> field <*> field <*> field <*> field <*> field

-- | Try to turn list of 'Int's to attributes
fromList :: [Int] -> Maybe Attributes
fromList xs
    | length xs == 6
    = let [str, dex, con, int, wis, cha] = xs
      in  Just (Attributes str dex con int wis cha)
    | otherwise
    = Nothing


-- | Throw 3D6 for each attribute
randomAttributes3D6 :: IO Attributes
randomAttributes3D6 = do
    attrs <- replicateM 6 (nTimesRoll 3 6)
    (return . fromJust . fromList . map sum) attrs

-- | Throw 4D6 and pick best of 3 for each attribute. Results in better
-- attributes on average
randomAttributes4D6BestOf3 :: IO Attributes
randomAttributes4D6BestOf3 = do
    attrs <- replicateM 6 (nTimesRoll 4 6)
    return
        $ (fromJust . fromList . map sum . map
              (take 3 . sortBy (flip $ compare))
          )
              attrs
