{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Queries
    ( randomAlignment
    , nRandomCharacters
    , defaultOptions
    )
where

import           Control.Monad
import           Database.SQLite.Simple
import           Database.SQLite.Simple.QQ
import           Paths_chargen                  ( getDataFileName )
import           Character
import           Character.Alignment
import           Character.Attributes    hiding ( str
                                                , dex
                                                , con
                                                , int
                                                , wis
                                                , cha
                                                )

data QueryOptions = QueryOptions { minLevel :: Int, maxLevel :: Int }
    deriving(Show, Eq)

-- | Default options for restricting query results
defaultOptions :: QueryOptions
defaultOptions = QueryOptions { minLevel = 1, maxLevel = 20 }

-- | Open connection to SQLite database conveniently
openChargenDb :: IO Connection
openChargenDb = getDataFileName "assets/chargen.db" >>= open

randomAlignment :: IO Alignment
randomAlignment = do
    connection <- openChargenDb
    [align]    <-
        query_
            connection
            [sql| SELECT alignment_abbrev FROM Alignment
                  ORDER BY RANDOM()
                  LIMIT 1;
            |] :: IO
            [Alignment]
    close connection
    return align

-- | Try to generate random 'Character' with given 'Attributes'
maybeGenerateCharacter
    :: Connection -> QueryOptions -> IO Attributes -> IO (Maybe Character)
maybeGenerateCharacter connection options attributeGen = do
    (Attributes str dex con int wis cha) <- attributeGen
    character                            <- queryNamed
        connection
        [sql|
            SELECT r.race_name, c.class_name,
                   (ABS(RANDOM()) % MIN(:maxLevel,
                            CASE WHEN xpt.max_level IS NULL THEN 9000 
                                 ELSE xpt.max_level END) + :minLevel),
                   a.alignment_abbrev, 
                   (:str + r.str_mod), (:dex + r.dex_mod), (:con + r.con_mod),
                   (:int + r.con_mod), (:wis + r.wis_mod), (:cha + r.cha_mod)
            FROM Alignment a, Class c, ClassAllowedAlignment cla, Race r,
                 RaceAllowedClass rac, XPTable xpt
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
            -- Check that attributes meet the requirements of the class
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
            AND xpt.xp_table_id = c.xp_table_id
            ORDER BY RANDOM()
            LIMIT 1;
            |]
        [ ":str" := str
        , ":dex" := dex
        , ":con" := con
        , ":int" := int
        , ":wis" := wis
        , ":cha" := cha
        , ":minLevel" := (1 :: Int)
        , ":maxLevel" := (20 :: Int)
        ]
    if null character then return Nothing else return $ Just (head character)

randomCharacter
    :: Connection    -- ^ Database connection
    -> QueryOptions  -- ^ Options to restrict query results
    -> IO Attributes -- ^ Method used to generate attributes. eg. 'randomAttributes3D6'
    -> IO Character  -- ^ Random character
randomCharacter connection options attributeGen = do
    char <- maybeGenerateCharacter connection options attributeGen
    case char of
        Just c  -> return c
        Nothing -> randomCharacter connection options attributeGen

nRandomCharacters
    :: Int              -- ^ Number of characters to generate
    -> QueryOptions  -- ^ Options to restrict query results
    -> IO Attributes    -- ^ Method to generate attributes
    -> IO [Character]   -- ^ Random characters
nRandomCharacters n options attributeGen = do
    conn  <- openChargenDb
    chars <- replicateM n $ randomCharacter conn options attributeGen
    close conn
    return chars

