{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DslrWWW.Database (
    buildDBString,
    runDB,
    checkPassword,
    insertUserHashPassword,
    getAllKeyframeLists,
    getSingleKeyframeList,
    insertKeyframeList,
    insertUserHashPassword
  ) where

import           DslrWWW.Types
import           DslrWWW.Database.Models
import           DslrWWW.Database.Marshal

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Class
import           Data.ByteString (ByteString)
import           Web.Heroku.Postgres
import           Data.Monoid
import           Data.Text.Encoding (encodeUtf8)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import           Control.Exception
import           Crypto.PasswordStore
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import           Control.Applicative
import qualified Data.Traversable as T

-- some settings to keep track of
hashStrength   = 14
pwToByteString = encodeUtf8

-- database functions

buildDBString = do
  params      <- dbConnParams 
  let connStr = foldr (\(k, v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " ")) "" params
  putStrLn . show $ connStr
  return connStr

runDB :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runDB query = do
  connStr <- buildDBString
  runResourceT . runStdoutLoggingT . withPostgresqlConn connStr $ runSqlConn query
  
-- general queries

checkPassword :: MonadIO m => Username -> Password -> ReaderT SqlBackend m Bool
checkPassword (Username name) (Password pwd) = do
  userEntity <- getBy (UniqueUsername name)
  return $ maybe False checkLogin userEntity
  where
    checkLogin (Entity _ (UserEntry _ _ _ _ hash)) = verifyPassword (pwToByteString pwd) hash

getAllKeyframeLists :: MonadIO m => Key UserEntry -> ReaderT SqlBackend m [(Key KeyframeListEntry, KeyframeList)]
getAllKeyframeLists userKey = do
  kfLists    <- selectList [KeyframeListEntryT_user ==. userKey] []
  kfContents <- sequence $ fmap (\(Entity id _) -> selectList [KeyframeEntryT_list ==. id] []) kfLists
  let kfEntities = fmap (fmap (\(Entity _ kfEntity) -> kfEntity)) kfContents
  let kfZip = zip kfLists kfEntities
  return $ fmap (\((Entity listId kflEntry), contents) -> (listId, entryToKfList kflEntry contents)) kfZip

getSingleKeyframeList uId kId = do
  user <- get uId
  kfl  <- get kId
  frames <- selectList [KeyframeEntryT_list ==. kId] []
  return $ do
    _ <- user
    k@(KeyframeListEntry uId' _) <- kfl
    if uId' /= uId
       then Nothing
       else return $ entryToKfList k $ fmap (\(Entity _ f) -> f) frames

-- update / delete functions

deleteKeyframeList :: (Applicative m, MonadIO m) => Key UserEntry -> Key KeyframeListEntry -> ReaderT SqlBackend m (Maybe ())
deleteKeyframeList userId kfListId = do
  maybeUser   <- get userId
  maybeKfList <- get kfListId
  T.traverse runDeletes $ maybeUser >>= (\_ -> maybeKfList) 
  where
    runDeletes (KeyframeListEntry userId' _) =
      if userId /= userId'
         then return ()
         else do
           deleteWhere [KeyframeEntryT_list ==. kfListId]
           delete kfListId
           
-- insertion functions

insertKeyframeList :: (MonadIO m, Applicative m) => Key UserEntry -> KeyframeList -> ReaderT SqlBackend m (Maybe (Key KeyframeListEntry))
insertKeyframeList userKey (KeyframeList kfName frames) = do  
  maybeUser <- get userKey
  T.sequence $ fmap insertFrames maybeUser
  where
    insertFrames _ = do
      kfListId <- insert $ KeyframeListEntry userKey kfName
      T.traverse (\(Keyframe pos pan tilt time) -> insert $ KeyframeEntry kfListId pos pan tilt time) frames
      return kfListId

insertUser :: MonadIO m => User -> ByteString -> ReaderT SqlBackend m (Key UserEntry)
insertUser (User (Username name) first last (Email mail)) hashedPassword = do
  userID <- insert $ UserEntry first last name mail hashedPassword
  return userID

insertUserHashPassword :: User -> Password -> IO (Key UserEntry)
insertUserHashPassword user (Password pw) = do
  passwordHash <- makePassword (pwToByteString pw) hashStrength
  runDB $ insertUser user passwordHash
