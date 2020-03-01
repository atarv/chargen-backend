{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Character.Alignment where

import           Data.Aeson
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           GHC.Generics
import           Data.Text                     as T
import           Text.Read                      ( readMaybe )
import           Control.Monad                  ( mzero )

data Alignment = LG -- ^ Lawful Good
               | LN -- ^ Lawful Neutral
               | LE -- ^ Lawful Evil
               | NG -- ^ Neutral Good
               | N  -- ^ (True) Neutral
               | NE -- ^ Neutral Evil
               | CG -- ^ Chaotic Good
               | CN -- ^ Chaotic Neutral
               | CE -- ^ Chaotic Evil
    deriving (Show, Read, Generic, Eq)

instance FromJSON Alignment where
    parseJSON (Object t) = fromAlignmentName <$> (t .: "value")
    parseJSON _          = mzero
instance ToJSON Alignment where
    toJSON al = toJSON $ alignmentName al
instance FromField Alignment where
    fromField (Field (SQLText t) mdata) =
        let alignment = (readMaybe . T.unpack) t :: Maybe Alignment
        in  case alignment of
                Just a  -> Ok a
                Nothing -> returnError ConversionFailed
                                       (Field (SQLText t) mdata)
                                       "need alignment abbreviation"
    fromField f = returnError ConversionFailed f "need alignment abbreviation"
instance FromRow Alignment where
    fromRow = field

-- | Convert Alignment to a readable name
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

fromAlignmentName :: T.Text -> Alignment
fromAlignmentName a = case a of
    "Lawful Good"     -> LG
    "Lawful Neutral"  -> LN
    "Lawful Evil"     -> LE
    "Neutral Good"    -> NG
    "Neutral"         -> N
    "Neutral Evil"    -> NE
    "Chaotic Good"    -> CG
    "Chaotic Neutral" -> CN
    "Chaotic Evil"    -> CE
    _                 -> error "Cannot convert to Alignment"
