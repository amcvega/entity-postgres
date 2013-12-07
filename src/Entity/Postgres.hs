module Entity.Postgres where

import Control.Applicative

import qualified Data.Text as T
import Data.Convertible
import Data.Time (timeZoneMinutes)
import Data.Char (toLower)
import Entity


import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.TypeInfo.Static


instance FromField StoreVal where
    fromField = storeParser



storeParser f mbs
    | t `elem` map typoid [text, varchar] = StoreText <$> fromField f mbs
    | t == typoid date = StoreDay <$> fromField f mbs
    | t `elem` map typoid [int8, int4, int2] =
        StoreInt <$> fromField f mbs
    | t `elem` map typoid [timestamptz] =
        StoreUTCTime <$> fromField f mbs
    | t `elem` map typoid [float8, float4] =
        StoreDouble <$> fromField f mbs
    | t == typoid numeric = StoreDouble <$> fromRational <$> fromField f mbs
    | otherwise =  returnError ConversionFailed f $  "invalid oid: " ++ (show t)
    where
        t = typeOid f



instance ToField StoreVal where
    toField x = case x of
        StoreInt i -> toField i
        StoreText t -> toField t
        StoreDay d -> toField d
        StoreDouble d -> toField d
        StoreUTCTime t -> toField t



instance (Storeable a) => ToRow (Entity a) where
    toRow = map (\(_,v) -> toField v) . toList . eVal


-- tableName x = storeName x
