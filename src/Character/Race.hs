{-# LANGUAGE DeriveGeneric #-}
module Character.Race where
import           Data.Aeson
import           GHC.Generics
import           Database.SQLite.Simple.FromField
import           Data.Set
import           Character.Class

data Race =
    UndefinedRace | Dwarf | Elf | Gnome | HalfElf | Halfling | HalfOrc | Human
    deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)

instance FromJSON Race
instance ToJSON Race
instance FromField Race where
    fromField field = do
        x <- fromField field
        return $ toEnum x -- Convert from `race_id` to Race instance

-- | Get classes permitted for given race.
allowedClasses :: Race -> Set Class
allowedClasses race = case race of
    UndefinedRace -> empty
    Dwarf         -> fromList [Assassin, Cleric, Fighter, Thief]
    Elf           -> fromList [Assassin, Cleric, Fighter, MagicUser, Thief]
    Gnome         -> fromList [Assassin, Cleric, Fighter, Illusionist, Thief]
    HalfElf -> fromList [Assassin, Cleric, Fighter, MagicUser, Ranger, Thief]
    Halfling      -> fromList [Fighter, Druid, Thief]
    HalfOrc       -> fromList [Assassin, Cleric, Fighter, Thief]
    Human         -> fromList [Assassin ..]
