{-# LANGUAGE DeriveGeneric #-}
module Character.Class where
import           Data.Aeson
import           GHC.Generics
import           Database.SQLite.Simple.FromField

-- TODO: Add rest of the character classes
data Class = Unused | Assassin | Cleric | Druid | Fighter
    deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)

instance FromJSON Class
instance ToJSON Class
instance FromField Class where
    fromField field = do
        x <- fromField field
        return $ toEnum x -- Convert from `class_id` to Class instance
