{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveGeneric #-}
module Queries
    ( nRandomCharacters
    , Queries.defaultOptions
    , QueryOptions(..)
    , validateQuery
    )
where

import           Control.Monad
import           Database.SQLite.Simple
import           Database.SQLite.Simple.QQ
import           Paths_chargen                  ( getDataFileName )
import           RandomUtil                     ( randInt )
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
import           Data.String
import           Data.List

-- | User given options and constraints for generating random characters
data QueryOptions =
    QueryOptions { count :: Int , minLevel :: Int, maxLevel :: Int
                 , selectedRaces :: [Race],  selectedClasses :: [Class]}
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

-- | Checks that query will have meaningful results
-- validateQuery :: IsString a => QueryOptions -> Either a QueryOptions
validateQuery (QueryOptions { count = c, minLevel = minL, maxLevel = maxL, selectedClasses = classes, selectedRaces = races })
    | c < 1
    = Left "Invalid count"
    | minL < 1 || minL > maxL
    = Left "Invalid level constraints"
    | null races
    = Left "No races selected"
    | null classes
    = Left "No classes selected"
    | otherwise
    = Right
        (QueryOptions { count           = c
                      , minLevel        = minL
                      , maxLevel        = maxL
                      , selectedClasses = classes
                      , selectedRaces   = races
                      }
        )

-- | Default options for restricting query results
defaultOptions :: QueryOptions
defaultOptions = QueryOptions
    { count           = 10
    , minLevel        = 1
    , maxLevel        = 20
    , selectedClasses = [(Assassin) .. ]
    , selectedRaces   = [(Dwarf) ..]
    }

-- | Open connection to SQLite database conveniently
openChargenDb :: IO Connection
openChargenDb = getDataFileName "assets/chargen.db" >>= open

-- | Try to generate random 'Character' with given 'Attributes'
maybeGenerateCharacter
    :: Connection -> QueryOptions -> IO Attributes -> IO (Maybe Character)
maybeGenerateCharacter connection options attributeGen = do
    (Attributes str dex con int wis cha) <- attributeGen -- generate attributes
    -- generate random level for character
    randLevel <- randInt (minLevel options, maxLevel options)
    character <- queryNamed
        connection
        -- TODO: Implement race and class constraints to SQL query
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
            -- Get class allowed for race at random
            AND c.class_id = (
                SELECT class_id FROM RaceAllowedClass
                WHERE r.race_id = race_id
                ORDER BY RANDOM()
                LIMIT 1
            )
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
        ]
    if null character then return Nothing else return $ Just (head character)

-- | Creates a random character (guaranteed)
randomCharacter
    :: Connection    -- ^ Database connection
    -> QueryOptions  -- ^ Options to restrict query results
    -> IO Attributes -- ^ Method used to generate attributes. eg. 
                     --   'randomAttributes3D6'
    -> IO Character  -- ^ Random character
randomCharacter connection options attributeGen = do
    char <- maybeGenerateCharacter connection options attributeGen
    case char of
        Just c  -> return c
        Nothing -> randomCharacter connection options attributeGen

-- | Creates a number of random characters
nRandomCharacters
    :: Int              -- ^ Number of characters to generate
    -> QueryOptions     -- ^ Options to restrict query results
    -> IO Attributes    -- ^ Method to generate attributes
    -> IO [Character]   -- ^ Random characters
nRandomCharacters n options attributeGen = do
    conn  <- openChargenDb
    chars <- replicateM n $ randomCharacter conn options attributeGen
    close conn
    return chars

