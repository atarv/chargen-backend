{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Queries where

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.QQ
import           Character.Alignment
import           Paths_chargen                  ( getDataFileName )

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
