{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Character.SavingThrows where

import           Data.Aeson
import           Database.SQLite.Simple.FromRow
import           GHC.Generics

data SavingThrows = SavingThrows { magicItems :: Int, breath :: Int
                                 , death :: Int, petrify :: Int, spells :: Int}
    deriving (Show, Read, Generic)

instance ToJSON SavingThrows
instance FromJSON SavingThrows
instance FromRow SavingThrows where
    fromRow = SavingThrows <$> field <*> field <*> field <*> field <*> field

fromList :: [Int] -> Maybe SavingThrows
fromList xs
    | length xs == 5
    = let [magic, breath, death, petrify, spells] = xs
      in  Just (SavingThrows magic breath death petrify spells)
    | otherwise
    = Nothing
