{-# LANGUAGE FlexibleInstances #-}
module DslrWWW.API (
    KeyframeAPI,
    keyframeAPI,
    keyframeEndpoints,
    getAllKeyframes,
    getKeyframesByID,
    postKeyframeList,
    addUser
  ) where

import           DslrWWW.Types
import qualified DslrWWW.Database as DB
import           DslrWWW.Database.Models

import           Data.ByteString (ByteString)
import           Servant
import           Servant.Docs
import           Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import           Servant.API.Experimental.Auth (AuthProtect)
import           Database.Persist.Sql
import           Control.Monad.IO.Class
import           Control.Monad.Except
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as T 

type KeyframeAPI = "api" :> (PublicAPI :<|> PrivateAPI)

keyframeEndpoints :: Server KeyframeAPI
keyframeEndpoints =
  ((uncurry addUser)
  :<|> login)
  :<|> getAllKeyframes
  :<|> getKeyframesByID 
  :<|> postKeyframeList

type PublicAPI =
       "user" :> "new"   :> ReqBody '[JSON] (User, Password) :> Post '[JSON] (UserSession)
--  :<|> "user" :> "login" :> BasicAuth "login" (Key UserEntry)                 :> Get  '[JSON] LoginToken
  :<|> "user" :> "login"                 :> Get  '[JSON] UserSession

type PrivateAPI =
       "all" :> Capture "userId" Integer                                     :> Get  '[JSON] [(KeyframeListId, KeyframeList)]
  :<|> "single" :> Capture "userId" Integer :> Capture "frameListID" Integer :> Get  '[JSON] (Maybe KeyframeList)
  :<|> "new" :> Capture "userId" Integer :> ReqBody '[JSON] KeyframeList     :> Post '[JSON] (Maybe KeyframeListId)

--apiContext :: Context '[BasicAuthCheck (Key UserEntry, LoginToken)]
--apiContext 

--loginContext :: Context '[BasicAuthCheck (Key UserEntry, LoginToken)]
--loginContext = authCheck :. EmptyContext       
       
-- more instance for documentation

{-authCheck :: BasicAuthCheck (Key UserEntry, LoginToken)
authCheck = BasicAuthCheck check where
  check (BasicAuthData name pw) = do
    loginResult <- DB.runDB $ DB.loginUser (Username $ T.decodeUtf8 name) (Password $ T.decodeUtf8 pw)
    case loginResult of
      Nothing  -> return Unauthorized
      Just res -> return $ Authorized res-}


instance ToCapture (Capture "userId" Integer) where
  toCapture _ = DocCapture "userId" "(integer) user id in database"

instance ToCapture (Capture "frameListID" Integer) where
  toCapture _ = DocCapture "frameListID" "(integer) keyframe list id in database"

keyframeAPI :: Proxy KeyframeAPI
keyframeAPI = Proxy

-- functions for each endpoint

getAllKeyframes :: MonadIO m  => Integer -> m [(KeyframeListId, KeyframeList)]
getAllKeyframes uId = liftIO $ do
  frames <- DB.runDB . DB.getAllKeyframeLists . UserEntryKey . fromIntegral $ uId
  return . fmap (\(key, list) -> (KeyframeListId . fromIntegral . unSqlBackendKey . unKeyframeListEntryKey $ key, list)) $ frames

getKeyframesByID :: MonadIO m => Integer -> Integer -> m (Maybe KeyframeList)
getKeyframesByID uId kId = liftIO $ do
  let userKey   = UserEntryKey $ fromIntegral uId
  let kfListKey = KeyframeListEntryKey $ fromIntegral kId
  DB.runDB $ DB.getSingleKeyframeList userKey kfListKey

postKeyframeList :: MonadIO m => Integer -> KeyframeList -> m (Maybe KeyframeListId)
postKeyframeList uId kfList = liftIO $ do
  let userKey = UserEntryKey $ fromIntegral uId
  kfKey <- DB.runDB $ DB.insertKeyframeList userKey kfList
  return $ fmap (KeyframeListId . fromIntegral . unSqlBackendKey . unKeyframeListEntryKey) kfKey

-- need to check how this could fail
addUser :: MonadIO m => User -> Password -> m UserSession
addUser user pw = do
  sessionKey <- liftIO $ DB.insertUserCreateSession user pw undefined undefined -- BAD BAD BAD
  return . UserSession . fromIntegral . unSqlBackendKey . unUserSessionEntryKey $ sessionKey
{-  (userKey, token) <- liftIO $ DB.insertUserHashPassword user pw 
  let userId = UserId . fromIntegral . unSqlBackendKey . unUserEntryKey $ userKey
  return (userId, token)-}
  

login :: MonadIO m => m UserSession
login = return $ undefined
