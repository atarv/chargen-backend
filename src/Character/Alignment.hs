{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Character.Alignment where

import           Data.Aeson
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           GHC.Generics
import           Data.Text                     as T
import           Text.Read (readMaybe)

data Alignment = LG | LN | LE | NG | N | NE | CG | CN | CE
    deriving (Show, Read, Generic, Eq)

instance FromJSON Alignment
instance ToJSON Alignment
instance FromField Alignment where
    fromField (Field (SQLText t) mdata) =
        let alignment = (readMaybe . T.unpack) t :: Maybe Alignment
        in  case alignment of
                Just a  -> Ok a
                Nothing -> returnError ConversionFailed
                                       (Field (SQLText t) mdata)
                                       "need alignment abbreviation"
    fromField f = returnError ConversionFailed f "need alignment abbreviation"

alignmentName :: Alignment -> T.Text
alignmentName a = case a of
    LG -> "Lawful Good"
    LN -> "Lawful Neutral"
    LE -> "Lawful Evil"
    NG -> "Neutral Good"
    N  -> "Neutral"
    NE -> "Neutral Evil"
    CG -> "Chaotic Good"
    CN -> "Chaotic Neutral"
    CE -> "Chaotic Evil"