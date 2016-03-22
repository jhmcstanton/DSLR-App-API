module DslrWWW.Database where

import           DslrWWW.Types
import           DslrWWW.Database.Models

import           Database.Persist
import           Database.Persist.Postgresql
import           Web.Heroku.Postgres
import           Data.Monoid
import           Data.Text.Encoding (encodeUtf8)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)

-- database functions

runDB :: SqlPersist (ResourceT IO) a -> IO a
runDB query = do
  params      <- dbConnParams
  let connStr = foldr (\(k, v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " ")) "" params
--  runResourceT . return . withPostgresqlConn connStr $ runSqlConn query
  undefined
