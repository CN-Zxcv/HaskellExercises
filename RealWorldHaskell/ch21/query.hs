
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

query :: Int -> IO ()
query maxId = do
    conn <- connectSqlite3 "test1.db"
    r <- quickQuery' conn
        "select id, desc from test where id <= ? ORDER BY id, desc"
        [toSql maxId]

    let stringRows = map convRow r

    mapM_ putStrLn stringRows

    disconnect conn

    where
        convRow :: [SqlValue] -> String
        convRow [sqlId, sqlDesc] = show intid ++ ": " ++ desc
            where
                intid = (fromSql sqlId) :: Integer
                desc = case fromSql sqlDesc of
                    Just x -> x
                    Nothing -> "NULL"
        convRow x = fail $ "Unexcepted result: " ++ show x
