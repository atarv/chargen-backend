{-# LANGUAGE DeriveGeneric #-}
module Character.Race where
import           Data.Aeson
import           GHC.Generics
import           Database.SQLite.Simple.FromField

-- TODO: Add rest of the races
data Race = UndefinedRace | Dwarf | Elf | Gnome
    deriving (Enum, Bounded, Eq, Show, Read, Generic)

instance FromJSON Race
instance ToJSON Race
instance FromField Race where
    fromField field = do
        x <- fromField field
        return $ toEnum x -- Convert from `race_id` to Race instance
