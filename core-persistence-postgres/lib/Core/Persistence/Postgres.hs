{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Reading and writing data to and from a Postgres database.

This module simply contains conveniences for using the most excellent
__postgresql-simple__ library in concert with the rest of the packages in
this collection.
-}
module Core.Persistence.Postgres (
    connectP,
    getConnection,
    getConnectionString,
    newQuery,
    newQuery_,
    queryP,
    queryP_,
    refreshConnection,
    setConnection,
    retryConnection,
    retryConnection_,
) where

import Control.Monad.IO.Class (liftIO)
import Core.Program (Program, getApplicationState, setApplicationState)
import Core.System (catch, throw)
import Core.Text (Rope, fromRope)
import Database.PostgreSQL.Simple (Connection, ExecStatus (..), FromRow, Query, SqlError (..), ToRow, connectPostgreSQL, query, query_)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField, toField)

instance FromField Rope where
    fromField f dat = fromRope <$> fromField f dat

instance ToField Rope where
    toField r = Escape $ fromRope r

class Database d where
    getConnection :: d -> Program d Connection
    setConnection :: Connection -> d -> Program d d
    getConnectionString :: d -> Program d Rope

queryP :: (ToRow q, FromRow r) => Connection -> Query -> q -> Program e [r]
queryP conn q args = liftIO $ query conn q args

queryP_ :: (FromRow r) => Connection -> Query -> Program e [r]
queryP_ conn q = liftIO $ query_ conn q

newQuery :: (Database d, ToRow q, FromRow r) => Query -> q -> Program d [r]
newQuery q args = do
    env <- getApplicationState
    conn <- getConnection env
    queryP conn q args

newQuery_ :: (Database d, FromRow r) => Query -> Program d [r]
newQuery_ q = do
    env <- getApplicationState
    conn <- getConnection env
    queryP_ conn q

connectP :: Rope -> Program d Connection
connectP connStr = liftIO $ connectPostgreSQL $ fromRope connStr

refreshConnection :: (Database d) => Program d Connection
refreshConnection = do
    env <- getApplicationState
    connStr <- getConnectionString env
    conn <- connectP connStr
    env' <- setConnection conn env
    setApplicationState env'
    return conn

connectionDisconnected :: SqlError
connectionDisconnected =
    SqlError
        { sqlState = ""
        , sqlExecStatus = FatalError
        , sqlErrorMsg = "connection disconnected"
        , sqlErrorDetail = ""
        , sqlErrorHint = ""
        }

-- These should be in the newQuery* series, but I haven't found a name I like
-- yet. Essentially, they're newQuery[_], but with a single round of connection
-- retry logic added in.
retryConnection :: (Database d, ToRow q, FromRow r) => Query -> q -> Program d [r]
retryConnection q args = do
    catch (newQuery q args) (helperFunction q args)
  where
    helperFunction :: (Database d, ToRow q, FromRow r) => Query -> q -> SqlError -> Program d [r]
    helperFunction q' args' ex = do
        if ex == connectionDisconnected
            then do
                _ <- refreshConnection
                newQuery q' args'
            else throw ex

retryConnection_ :: (Database d, FromRow r) => Query -> Program d [r]
retryConnection_ q = do
    catch (newQuery_ q) (helperFunction q)
  where
    helperFunction :: (Database d, FromRow r) => Query -> SqlError -> Program d [r]
    helperFunction q' ex = do
        if ex == connectionDisconnected
            then do
                _ <- refreshConnection
                newQuery_ q'
            else throw ex
