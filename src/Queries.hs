{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Queries
    ( randomAlignment
    , randomCharacter
    , maybeGenerateCharacter
    )
where

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

-- | Try to generate a random 'Character' with given 'Attributes'
maybeGenerateCharacter :: Attributes -> IO (Maybe Character)
maybeGenerateCharacter (Attributes str dex con int wis cha) = do
    connection <- openChargenDb
    character  <- queryNamed
        connection
        [sql|
            SELECT r.race_name, c.class_name, a.alignment_abbrev, 
                   (:str + r.str_mod), (:dex + r.dex_mod), (:con + r.con_mod),
                   (:int + r.con_mod), (:wis + r.wis_mod), (:cha + r.cha_mod)
            FROM Alignment a, Class c, ClassAllowedAlignment cla, Race r,
                 RaceAllowedClass rac
            -- Attributes must be between requirements of race
            WHERE (:str + r.str_mod) BETWEEN r.str_min AND r.str_max
            AND (:dex + r.dex_mod) BETWEEN r.dex_min AND r.dex_max
            AND (:con + r.con_mod) BETWEEN r.con_min AND r.con_max
            AND (:int + r.int_mod) BETWEEN r.int_min AND r.int_max
            AND (:wis + r.wis_mod) BETWEEN r.wis_min AND r.wis_max
            AND (:cha + r.cha_mod) BETWEEN r.cha_min AND r.cha_max
            -- Get only classes allowed by race
            AND r.race_id = rac.race_id
            AND rac.class_id = c.class_id
            -- Check that attributes meet the requirements of the class
            AND (:str + r.str_mod) >= c.str_min
            AND (:dex + r.dex_mod) >= c.dex_min
            AND (:con + r.con_mod) >= c.con_min
            AND (:int + r.int_mod) >= c.int_min
            AND (:wis + r.wis_mod) >= c.wis_min
            AND (:cha + r.cha_mod) >= c.cha_min
            -- Alignments allowed for class
            AND c.class_id = cla.class_id
            AND a.alignment_abbrev = cla.alignment_abbrev
            ORDER BY RANDOM()
            LIMIT 1;
            |]
        [ ":str" := str
        , ":dex" := dex
        , ":con" := con
        , ":int" := int
        , ":wis" := wis
        , ":cha" := cha
        ]
    close connection
    if null character then return Nothing else return $ Just (head character)

randomCharacter
    :: IO Attributes -- ^ Method used to generate attributes. eg. 'randomAttributes3D6'
    -> IO Character  -- ^ Random character
randomCharacter attributeGen = do
    attr <- attributeGen
    char <- maybeGenerateCharacter attr
    case char of
        Just c  -> return c
        Nothing -> randomCharacter attributeGen

