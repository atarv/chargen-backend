{-# LANGUAGE DeriveGeneric #-}
module Character where

import           Data.Aeson
import           Database.SQLite.Simple.FromRow
import           GHC.Generics
import           Character.Attributes
import           Character.Alignment
import           Character.SavingThrows

-- | Represents in-game character
data Character = Character {race :: String, cClass :: String, level :: Int
                           , alignment :: Alignment, attributes :: Attributes}
    deriving (Show, Read, Generic)

instance ToJSON Character
instance FromJSON Character
instance FromRow Character where
    fromRow = Character <$> field <*> field <*> field <*> field <*> fromRow
