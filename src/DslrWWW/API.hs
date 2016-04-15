{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module DslrWWW.API (
    KeyframeAPI,
    keyframeAPI,
    keyframeEndpoints,
--    serverContext,
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
import           Control.Monad.Trans.Except
import           Crypto.JWT
import           Crypto.JOSE.JWS
import           Crypto.JOSE.JWK
import           Crypto.JOSE.Error (Error(..))
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.HashMap.Strict as HM
import           Data.Time.Clock (addUTCTime, UTCTime)
import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import           Data.Aeson (toJSON, fromJSON, Result(..))
import           Data.Default.Class

issName, idKey :: T.Text
issName = "pbjdollys.com"
idKey   = "userid"

type KeyframeAPI = "api" :> (PublicAPI :<|> PrivateAPI)

serverContext :: Context (BasicAuthCheck UserId ': '[])
serverContext = loginCheck :. EmptyContext

keyframeEndpoints :: Server KeyframeAPI
keyframeEndpoints =
  ((uncurry addUser))
  -- :<|> login)
  :<|> getAllKeyframes
  :<|> getKeyframesByID 
  :<|> postKeyframeList

type PublicAPI =
       "user" :> "new"   :> ReqBody '[JSON] (User, Password)                  :> Post '[JSON] UserId
--  :<|> "user" :> "login" :> BasicAuth "login" User                             :> Get  '[JSON] JWT

type PrivateAPI =
       "all" :> Capture "userId" Integer                                     :> Get  '[JSON] [(KeyframeListId, KeyframeList)]
  :<|> "single" :> Capture "userId" Integer :> Capture "frameListID" Integer :> Get  '[JSON] (Maybe KeyframeList)
  :<|> "new" :> Capture "userId" Integer :> ReqBody '[JSON] KeyframeList     :> Post '[JSON] (Maybe KeyframeListId)
       
-- more instance for documentation


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
addUser :: MonadIO m => User -> Password -> m UserId
addUser user pw = do
  userKey <- liftIO $ DB.insertUserHashPassword user pw 
  let userId = UserId . fromIntegral . unSqlBackendKey . unUserEntryKey $ userKey
  return userId


loginCheck :: BasicAuthCheck UserId
loginCheck = BasicAuthCheck check where
  check (BasicAuthData username password) = do
    maybeUid <- DB.runDB $ DB.checkPassword (Username $ T.decodeUtf8 username) (Password $ T.decodeUtf8 password)
    case maybeUid of
      Nothing    -> return Unauthorized
      (Just uid) -> return . Authorized . UserId . fromIntegral . unSqlBackendKey . unUserEntryKey $ uid

loginHandler :: (MonadIO m, MonadRandom (ExceptT ServantErr m)) => JWK -> JWSHeader -> UserId -> ExceptT ServantErr m JWT
loginHandler jwk header uid = do
  curTime <- liftIO getPOSIXTime
  token <- mkToken jwk header (posixSecondsToUTCTime curTime) uid
  case token of
    Left _ -> throwE err500 
    Right token -> return token


validateJWT :: MonadRandom m => JWK -> JWT -> m Bool
validateJWT jwk jwt = do
--  jwk <- genJWK $ RSAGenParam 2048
  return (validateJWSJWT def AnyValidated jwk jwt)

mkToken :: MonadRandom m => JWK -> JWSHeader -> UTCTime -> UserId -> m (Either Error JWT)
mkToken secret header curTime userId = createJWSJWT secret header claims where
  claims = emptyClaimsSet {
    _claimIss = Just (fromString issName),
    _claimExp  = (Just $ NumericDate $ addUTCTime (24 * 60 * 60) curTime),
    _unregisteredClaims = HM.fromList [(idKey, toJSON userId)]
    }

getIdFromToken :: JWT -> Result UserId 
getIdFromToken jwt = fromJSON $ (_unregisteredClaims . jwtClaimsSet $ jwt ) HM.! idKey
