{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveGeneric, RecordWildCards #-}
module Queries
    ( AttributeGenMethod(..)
    , generateAttributesWith
    , nRandomCharacters
    , Queries.defaultOptions
    , QueryOptions(..)
    , validateQuery
    )
where

import           Control.Monad
import qualified Data.Scientific               as Scientific
import           Database.SQLite.Simple
import           Database.SQLite.Simple.QQ
import           Paths_chargen                  ( getDataFileName )
import           RandomUtil                     ( randInt
                                                , chooseFromSet
                                                )
import           Character
import           Character.Attributes    hiding ( str
                                                , dex
                                                , con
                                                , int
                                                , wis
                                                , cha
                                                )
import           Character.Class
import           Character.Race
import           GHC.Generics
import           Data.Aeson
import           Data.Set                      as Set
import           System.Environment             ( getArgs )
import           Control.Exception              ( bracket
                                                , catch
                                                )

-- | User given options and constraints for generating random characters
data QueryOptions =
    QueryOptions { count :: Int , minLevel :: Int, maxLevel :: Int
                 , selectedRaces :: Set Race,  selectedClasses :: Set Class
                 , attributeGen :: AttributeGenMethod
                 }
    deriving(Show, Read, Eq, Generic)


instance ToJSON QueryOptions
instance FromJSON QueryOptions where
    parseJSON = withObject "QueryOptions" $ \q ->
        QueryOptions
            <$> q
            .:  "count"
            <*> q
            .:  "minLevel"
            <*> q
            .:  "maxLevel"
            <*> q
            .:  "selectedRaces"
            <*> q
            .:  "selectedClasses"
            <*> q
            .:  "attributeGen"

data AttributeGenMethod = Method3D6 | Method4D6BestOf3
    deriving (Show, Read, Eq, Generic)

instance FromJSON AttributeGenMethod
instance ToJSON AttributeGenMethod

generateAttributesWith :: AttributeGenMethod -> IO Attributes
generateAttributesWith a | a == Method3D6 = randomAttributes3D6
                         | otherwise      = randomAttributes4D6BestOf3

-- | Checks that query will have meaningful results
validateQuery :: QueryOptions -> Either String QueryOptions
validateQuery opts@QueryOptions {..}
    | count < 1
    = Left "Count too low"
    | count > 1000
    = Left "Count too high"
    | minLevel < 1 || minLevel > maxLevel
    = Left "Invalid level constraints"
    | Set.null selectedRaces
    = Left "No races selected"
    | Set.null selectedClasses
    = Left "No classes selected"
    | Unused `Set.member` selectedClasses
    = Left "Forbidden class"
    | UndefinedRace `Set.member` selectedRaces
    = Left "Forbidden race"
    | Set.disjoint selectedClasses $ Set.foldl
        (\cls r -> Set.union (allowedClasses r) cls)
        Set.empty
        selectedRaces
    = Left "No permitted class selected for given races"
    | otherwise
    = Right opts

-- | Default options for restricting query results
defaultOptions :: QueryOptions
defaultOptions = QueryOptions { count           = 10
                              , minLevel        = 1
                              , maxLevel        = 20
                              , selectedClasses = Set.fromList [Assassin ..]
                              , selectedRaces   = Set.fromList [Dwarf ..]
                              , attributeGen    = Method3D6
                              }

-- | Try to generate random 'Character' with given 'Attributes'.
--   This may recurse infinitely with certain options combinations.
generateCharacter :: Connection -> QueryOptions -> IO Character
generateCharacter connection options@QueryOptions {..} = do
    (Attributes str dex con int wis cha) <- generateAttributesWith attributeGen
    -- Generate random level for character, possibly overridden by class maximum
    -- level
    randLevel                            <- randInt (minLevel, maxLevel)
    -- Choose race and class at random with restrictions given in options
    chosenRace                           <- chooseFromSet $ Set.filter
        (not . Set.disjoint selectedClasses . allowedClasses)
        selectedRaces
    chosenClass <-
        (chooseFromSet . Set.intersection selectedClasses . allowedClasses)
            chosenRace
    character <- queryNamed
        connection
        [sql|
            SELECT r.race_name, c.class_id, 
                MIN(:randLevel, CASE WHEN xpt.max_level IS NULL 
                    THEN 9000 ELSE xpt.max_level END) as level,
                a.alignment_abbrev, 
                (:str + r.str_mod), (:dex + r.dex_mod), (:con + r.con_mod),
                (:int + r.con_mod), (:wis + r.wis_mod), (:cha + r.cha_mod),
                strow.magic_items, strow.breath, strow.death, strow.petrify,
                strow.spells
            FROM Alignment a, Class c, ClassAllowedAlignment cla, Race r,
                 RaceAllowedClass rac, XPTable xpt, SavingThrowTable stt,
                 SavingThrowRow strow
            -- Attributes must be between requirements of race
            WHERE (:str + r.str_mod) BETWEEN r.str_min AND r.str_max
            AND (:dex + r.dex_mod) BETWEEN r.dex_min AND r.dex_max
            AND (:con + r.con_mod) BETWEEN r.con_min AND r.con_max
            AND (:int + r.int_mod) BETWEEN r.int_min AND r.int_max
            AND (:wis + r.wis_mod) BETWEEN r.wis_min AND r.wis_max
            AND (:cha + r.cha_mod) BETWEEN r.cha_min AND r.cha_max
            AND r.race_id = :raceId
            AND c.class_id = :classId
            -- Check that attributes meet the requirements of the class with
            -- racial modifiers applied
            AND (:str + r.str_mod) >= c.str_min
            AND (:dex + r.dex_mod) >= c.dex_min
            AND (:con + r.con_mod) >= c.con_min
            AND (:int + r.int_mod) >= c.int_min
            AND (:wis + r.wis_mod) >= c.wis_min
            AND (:cha + r.cha_mod) >= c.cha_min
            -- Get allowed alignment at random
            AND a.alignment_abbrev = (
                SELECT alignment_abbrev FROM ClassAllowedAlignment
                WHERE c.class_id = class_id
                ORDER BY RANDOM()
                LIMIT 1
            )
            -- Get saving throws with respect to class and randLevel
            AND xpt.xp_table_id = c.xp_table_id
            AND stt.class_id = c.class_id
            AND strow.st_table_id = stt.st_table_id
            AND strow.min_level = (SELECT MAX(min_level) FROM SavingThrowRow
                WHERE min_level <= level
                AND st_table_id = stt.st_table_id
                AND stt.class_id = c.class_id)
            ORDER BY RANDOM()
            LIMIT 1;
            |]
        [ ":str" := str
        , ":dex" := dex
        , ":con" := con
        , ":int" := int
        , ":wis" := wis
        , ":cha" := cha
        , ":randLevel" := randLevel
        , ":raceId" := fromEnum chosenRace
        , ":classId" := fromEnum chosenClass
        ]
    if Prelude.null character
        then generateCharacter connection options
        else return $ head character

-- | Creates a number of random characters
nRandomCharacters
    :: String           -- ^ Path to character database (sqlite)
    -> Int              -- ^ Number of characters to generate
    -> QueryOptions     -- ^ Options to restrict query results
    -> IO [Character]   -- ^ Random characters
nRandomCharacters dbPath n options = bracket
    (open dbPath)
    close
    (\conn -> replicateM n $ generateCharacter conn options)
