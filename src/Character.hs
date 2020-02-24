{-# LANGUAGE DeriveGeneric #-}
module Character where

import           Data.Aeson
import           Database.SQLite.Simple.FromRow
import           GHC.Generics
import           Character.Attributes
import           Character.Alignment
import           Character.SavingThrows
import           Character.Class

-- | Represents in-game character
data Character = Character {race :: String, cClass :: Class, level :: Int
                           , alignment :: Alignment, attributes :: Attributes
                           , savingThrows :: SavingThrows }
    deriving (Show, Read, Generic)

instance ToJSON Character
instance FromJSON Character
instance FromRow Character where
    fromRow =
        Character
            <$> field
            <*> field
            <*> field
            <*> field
            <*> fromRow
            <*> fromRow
