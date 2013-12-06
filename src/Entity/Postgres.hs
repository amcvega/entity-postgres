module Entity.Postgres where

import qualified Data.Text as T
import Data.Convertible
import Data.Time (timeZoneMinutes)
import Data.Char (toLower)
import Entity


import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
-- import Database.SQLite.Simple
-- import Database.SQLite.Simple.FromField
-- import Database.SQLite.Simple.ToField


instance FromField StoreVal where
    fromField = undefined -- return . convert . fieldData


instance ToField (Filter a) where
    toField = undefined -- toField (Filter _ val) = convert . toStore $ val

-- instance Convertible StoreVal SQLData where
    -- safeConvert val = return $ case val of
    --     StoreInteger x -> SQLInteger $ fromInteger x
    --     StoreInt x -> SQLInteger $ fromIntegral x
    --     StoreDouble x -> SQLFloat x
    --     StoreText x -> SQLText x
    --     StoreString x -> SQLText $ T.pack x
    --     StoreBool x -> SQLInteger $ case x of
    --         True -> 1
    --         False -> 0
    --     StoreUTCTime x -> SQLFloat $ convert x
    --     StoreDay x -> SQLInteger $
    --                   fromIntegral (convert x :: Integer )
    --     StoreTimeZone x -> SQLInteger $ fromIntegral (timeZoneMinutes x)
    --     StoreByteString x -> SQLBlob x
    --     StoreNil -> SQLBlob ""


-- instance Convertible SQLData StoreVal where
--     safeConvert val = case val of
--         SQLInteger x -> return $ StoreInt $ fromIntegral x
--         SQLFloat x -> return $ StoreDouble x
--         SQLText x  -> return $ StoreText x
--         SQLBlob x -> return $ StoreByteString x
--         SQLNull -> return $ StoreNil




-- instance (Storeable a) => ToRow (Entity a) where
--     toRow = map (\(_,v) -> convert v) . toList . eVal


-- tableName x = storeName x
