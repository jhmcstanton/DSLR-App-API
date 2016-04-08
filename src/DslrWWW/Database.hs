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
    insertUserHashPassword,
    insertUserCreateSession
  ) where

import           DslrWWW.Types
import           DslrWWW.Database.Models
import           DslrWWW.Database.Marshal

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Class
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Base64
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
import           Data.Time.Clock (addUTCTime)
import           Data.Time.Clock.POSIX

-- some settings to keep track of
hashStrength   = 14
pwToByteString = encodeUtf8

-- database functions

buildDBString = do
  params      <- dbConnParams 
  let connStr = foldr (\(k, v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " ")) "" params
  return connStr

runDB :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runDB query = do
  connStr <- buildDBString
  runResourceT . runStdoutLoggingT . withPostgresqlConn connStr $ runSqlConn query
  
-- general queries

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


-- user functions

{-insertLoginKey :: MonadIO m => UserEntryId -> ReaderT SqlBackend m (LoginToken)
insertLoginKey uid = do
  time <- liftIO getPOSIXTime
  let validEndTime = time + posixDayLength
  token <- liftIO $ makePassword (pack $ show time ++ "mzns'lKjHaalBYlamABqhshAYqjAGmcBUqkh1i(A12j28AKu721k") hashStrength
  let tokenEntry = LoginTokenEntry uid (round validEndTime) (encode token)
  insert $ tokenEntry
  return $ entryToLoginToken tokenEntry-}

insertUserCreateSession :: User -> Password -> IPAddress -> UserAgent -> IO (Key UserSessionEntry)
insertUserCreateSession user password ip agent = do
  uid <- insertUserHashPassword user password
  sid <- runDB $ createSession uid ip agent -- super gross hack
  return $ sid

createSession :: MonadIO m => UserEntryId -> IPAddress -> UserAgent -> ReaderT SqlBackend m (Key UserSessionEntry)
createSession uid (IPAddress (fst, snd, thd, fth)) (UserAgent agent) = do
  curTime     <- liftIO $ getPOSIXTime
  let expires = utcTimeToPOSIXSeconds $ addUTCTime (fromIntegral $ 24 * 60 * 60) (posixSecondsToUTCTime curTime)
  deleteBy $ UniqueOwner uid
  session     <- insert $ UserSessionEntry uid fst snd thd fth agent (truncate expires)
  Just (Entity sessionID _) <- getBy $ UniqueOwner uid
  
  return  sessionID

insertUser :: MonadIO m => User -> ByteString -> ReaderT SqlBackend m (Key UserEntry)
insertUser (User (Username name) first last (Email mail)) hashedPassword = do
  userID <- insert $ UserEntry first last name mail hashedPassword
  return userID

insertUserHashPassword :: User -> Password -> IO (Key UserEntry)
insertUserHashPassword user (Password pw) = do
  passwordHash <- makePassword (pwToByteString pw) hashStrength
  runDB $ insertUser user passwordHash -- >>= (\uid -> insertLoginKey uid >>= \key -> return (uid, key))

{-loginUser :: (Functor m, MonadIO m) => Username -> Password -> ReaderT SqlBackend m (Maybe (Key UserEntry, LoginToken))
loginUser username pw = do
  verified <- checkPassword username pw
  case verified of
    Nothing  -> return Nothing
    Just uid -> do
      tokenEntity <- getBy (UniqueUser uid)
      case tokenEntity of
        Nothing -> fmap (wrapResult uid) . insertLoginKey $ uid 
        Just (Entity _ entry) -> return . wrapResult uid . entryToLoginToken $ entry
   where
     wrapResult uid token = Just (uid, token)-}
                  
  
checkPassword :: MonadIO m => Username -> Password -> ReaderT SqlBackend m (Maybe (Key UserEntry))
checkPassword (Username name) (Password pwd) = do
  userEntity <- getBy (UniqueUsername name)
  return $ do
    (Entity uid (UserEntry _ _ _ _ hash)) <- userEntity
    if verifyPassword (pwToByteString pwd) hash
       then return uid
       else Nothing
