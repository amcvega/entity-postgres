{-# LANGUAGE OverloadedStrings, RecordWildCards, GADTs, FlexibleContexts #-}
module Entity.Postgres.Interface where


import Data.Convertible (Convertible)
import Data.Data (Typeable)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Either

import Text.Printf (printf)

import Database.PostgreSQL.Simple as PQ
import Database.PostgreSQL.Simple.Types (Query(..))
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow (toRow)

import Entity.Postgres.Instances


import Entity hiding (query)


evalCommonPG :: Connection -> CommonI a -> IO (Either String a)
evalCommonPG db fn = case fn of
    FindRecord x -> findRecord db x
    CreateRecord plan -> createRecord db plan
    UpdateRecord old new -> updateRecord db old new
    DeleteRecord e -> deleteRecord db e
    FindRecordsByIndices filters -> findRecordsMulti db filters
    FilterQuery q -> filterQuery db q
    FindRecordByUnique attr val -> findRecordByUnique db attr val


filterStore :: [Filter a] -> a
filterStore = undefined

keyStore :: Key a -> a
keyStore = undefined

findRecordsMulti ::(Storeable a)
                   => Connection
                   -> [Filter a] -> IO (Either String [Entity a])
findRecordsMulti db fs = undefined
    -- let sql = "SELECT " ++ genFields (fieldStore f)
    --            ++  " FROM "
    --            ++ storeName (fieldStore f) ++ " WHERE " ++
    --            fieldOf f ++ " = ? LIMIT 1"
    --     finQry = Query $ B.pack sql
    -- res:_ <- PQ.query db finQry (Only $ toStore val)
    -- return $ Right res



deleteRecord db (Entity eid old)= do
    let sql = "DELETE FROM " ++ storeName old ++
              " WHERE id = ?"
        finSql = Query $ B.pack sql
    _ <- PQ.execute db finSql (Only $ unKey eid)
    return $ Right ()
    where
        updateFields a = intercalate ", " $
              map (\(Field x) -> fieldOf x ++ "=? ") (storeFields a)

findRecordByUnique :: (Storeable a, Convertible val StoreVal)
                      => Connection
                      -> StoreField a val
                      -> val
                      -> IO (Either String (Entity a))
findRecordByUnique db f val = do
    let sql = "SELECT " ++ genFields (fieldStore f)
               ++  " FROM "
               ++ storeName (fieldStore f) ++ " WHERE " ++
               fieldOf f ++ " = ? LIMIT 1"
        finQry = Query $ B.pack sql
    res:_ <- PQ.query db finQry (Only $ toStore val)
    return $ Right res
    -- return $ Right $ Entries res


updateRecord db (Entity oid old) new = do
    let sql = "UPDATE " ++ storeName old ++ " SET " ++
              updateFields old ++ " WHERE id = ?"
        finSql = Query $ B.pack sql
    _ <- PQ.execute db finSql (toRow new ++ [toField (unKey oid)] )
    return $ Right new
    where
        updateFields a = intercalate ", " $
              map (\(Field x) -> fieldOf x ++ "=? ") (storeFields a)

filterQuery :: (Storeable a)
               => Connection
               -> SimpleQuery a
               -> IO (Either String (QueryResult a))
filterQuery db qry = do
    let sql = "SELECT " ++ selectFor qry
               ++  " FROM "
               ++ queryStore qry ++ " "
        sql' = sql ++ filterClause qry ++ sortClause qry ++ limitClause qry
        finQry = Query $ B.pack sql'
    case qResult qry of
        ResultAll -> do
            res <- PQ.query db finQry (filterFieldsFor qry)
            return $ Right $ Entries res
        ResultCount -> do
            res <- PQ.query db finQry (filterFieldsFor qry)
            let Only z = head res
            return $ Right $ Count z
    where
        filterFieldsFor q =
            concatMap filterParse (qInt q ++ qUnion q)
            where
                filterParse (Filter _ v) = [toStore v]
                filterParse (RangeFilter _ x y) = [toStore x, toStore y]
        selectFor q = case qResult q of
            ResultAll ->  genFields (querySubject q)
            ResultCount -> " COUNT(*) "


genFields :: Storeable a =>  a -> String
genFields a = intercalate ", " $
              "id" : map (\(Field x) -> fieldOf x) (storeFields a)

limitClause :: SimpleQuery a -> String
limitClause qry = case qLimit qry of
    NoLimit -> ""
    Limit off lim -> printf " LIMIT %d OFFSET %d " lim off

sortClause :: SimpleQuery a -> String
sortClause qry = case qSort qry of
    [] -> ""
    fs -> "ORDER BY " ++ intercalate "," (map clause fs)
    where clause (Sort dir f) = case dir of
              Desc -> fieldOf f ++ " DESC"
              Asc -> fieldOf f ++ " ASC"

filterClause :: SimpleQuery a -> String
filterClause SimpleQuery{..} = prefix ++  intersects ++ iuJoin ++ unions
    where intersects = whereClause qInt
          unions = unionClause qUnion
          iuJoin = case qUnion ++ qInt of
              [] -> ""
              _ -> " AND "
          prefix = case qInt ++ qUnion ++ qNot of
              [] -> ""
              _ -> " WHERE "

unionClause :: [Filter a] -> String
unionClause [] = ""
unionClause fs = "( " ++ intercalate " OR " (map filterClause' fs) ++ " )"
    -- where
    --     clause (Filter x _) = let name = fieldOf x
    --                           in "`" ++ name ++ "` = ?"



findByIndices :: (Storeable a)
                 => Connection -> [Filter a] -> IO (Either String [Entity a])
findByIndices db filters = do
    let qry' = "SELECT * FROM " ++ table ++ " " ++ whereClause filters
        qry = Query $ B.pack qry'
        table = storeName $ filterStore filters
    rawRes <- query db qry $ filters
    let entsE = map sqlToEntity $ map valsToList rawRes
        errs = lefts entsE
        ents = rights entsE
    if null errs
       then return $ Right ents
        else return $ Left $ show errs
    where
        valsToList (x:xs) =
            let flds = map (\(Field x') -> fieldOf x') sfields
                sfields =  storeFields $ filterStore filters
            in
             ("id",x) : zip flds xs

findRecord :: (Storeable a) => Connection -> Key a -> IO (Either String (Entity a))
findRecord db kid = do
    let qry = Query $ B.pack $ "SELECT "
                               ++ genFields obj
                               ++ " FROM " ++ table
                               ++ " WHERE `id`= ? LIMIT 1"
        table = storeName $ keyStore kid
        obj = keyStore kid
    rawRes <- PQ.query db qry [unKey kid]
    let entsE = map sqlToEntity $ map valsToList rawRes
        ents = rights entsE
    return $ Right $ head ents
    where
        valsToList (x:xs) =
            let flds = map (\(Field x') -> fieldOf x') sfields
                sfields =  storeFields $ keyStore kid
            in
             ("id",x) : zip flds xs


whereClause :: [Filter a] -> String
whereClause [] = ""
whereClause fs = "( " ++ intercalate " AND " (map filterClause' fs) ++ " )"


filterClause' (Filter x _) = let name = fieldOf x
                             in "`" ++ name ++ "` = ?"
filterClause' (RangeFilter x _ _) = let name = fieldOf x
                                    in "(`" ++ name ++ "` >= ? AND `" ++
                                       name ++ "` <= ?)"




sqlToEntity :: (Typeable a, Storeable a)
               => [(String, StoreVal)] -> Either String (Entity a)
sqlToEntity [] = error "Empty Result."
sqlToEntity (x:xs) =
    Entity (fromStore $ snd x) `fmap` (listToStoreable xs)



createRecord :: (Storeable a) => Connection -> a -> IO (Either String (Key a))
createRecord db obj = do
    let sqlobj = Entity undefined obj
        fields = map getname $ storeFields obj
        table = storeName obj
        query' = "INSERT INTO " ++ table ++ " ( " ++ toValues fields ++ " )" ++
                " VALUES ( " ++ qmarks fields ++ " ) RETURNING id "
        qry = Query $ B.pack query'
    [Only lid] <- returning db qry [sqlobj]
    return . Right $ Key lid
    where
        getname (Field x) = fieldOf x
        toValues xs = intercalate " , " xs
        qmarks xs = intercalate ", " $ replicate (length xs) "?"
