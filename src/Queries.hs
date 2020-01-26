{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Queries where

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.QQ
import           Paths_chargen                  ( getDataFileName )
import Character
import           Character.Alignment
import Character.Attributes


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
            |] :: IO [Alignment]
    close connection
    return align

randomCharacter :: Attributes -> IO Character
randomCharacter (Attributes str dex con int wis cha) = do
      connection <- openChargenDb
      [character] <- queryNamed connection
            [sql|
            SELECT r.race_name, c.class_name, a.alignment_abbrev, :str, :dex, :con, :int, :wis, :cha
            FROM Alignment a, Class c, ClassAllowedAlignment cla, Race r, RaceAllowedClass rac
            WHERE :str BETWEEN r.str_min AND r.str_max
            AND :dex BETWEEN r.dex_min AND r.dex_max
            AND :con BETWEEN r.con_min AND r.con_max
            AND :int BETWEEN r.int_min AND r.int_max
            AND :wis BETWEEN r.wis_min AND r.wis_max
            AND :cha BETWEEN r.cha_min AND r.cha_max
            AND r.race_id = rac.race_id
            AND rac.class_id = c.class_id
            AND :str >= c.str_min
            AND :dex >= c.dex_min
            AND :con >= c.con_min
            AND :int >= c.int_min
            AND :wis >= c.wis_min
            AND :cha >= c.cha_min
            AND c.class_id = cla.class_id
            AND a.alignment_abbrev = cla.alignment_abbrev
            ORDER BY RANDOM()
            LIMIT 1;
            |]
            [":str" := str, ":dex" := dex, ":con" := con, ":int" := int, ":wis" := wis, ":cha" := cha]
      return character


