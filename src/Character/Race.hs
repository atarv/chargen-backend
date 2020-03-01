{-# LANGUAGE DeriveGeneric #-}
module Character.Race where
import           Data.Aeson
import           GHC.Generics
import           Database.SQLite.Simple.FromField
import           Data.Set
import           Character.Class

-- TODO: Add rest of the races
data Race = UndefinedRace | Dwarf | Elf | Gnome
    deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)

instance FromJSON Race
instance ToJSON Race
instance FromField Race where
    fromField field = do
        x <- fromField field
        return $ toEnum x -- Convert from `race_id` to Race instance

allowedClasses race = case race of
    UndefinedRace -> empty
    Dwarf         -> fromList [Assassin, Cleric, Fighter]
    Elf           -> fromList [Assassin, Cleric, Fighter]
    Gnome         -> fromList [Assassin, Cleric, Fighter]
