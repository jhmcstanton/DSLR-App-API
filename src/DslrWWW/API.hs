{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DslrWWW.API (
    KeyframeAPI,
    apiToJS,
    keyframeAPI,
    keyframeEndpoints,
    serverContext,
    getAllKeyframes,
    getKeyframesByID,
    postKeyframeList,
    addUser
  ) where

import           DslrWWW.Types
import qualified DslrWWW.Database as DB
import           DslrWWW.Database.Models

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import           Servant
import           Servant.Docs
import           Servant.Docs.Internal
import           Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import           Servant.API.Experimental.Auth (AuthProtect)
import           Database.Persist.Sql
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Crypto.JWT
import           Crypto.JOSE.JWS
import           Crypto.JOSE.JWK
import           Crypto.JOSE.Compact 
import           Crypto.JOSE.Error (Error(..))
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.HashMap.Strict as HM
import           Data.Time.Clock (addUTCTime, UTCTime)
import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import           Data.Aeson (toJSON, fromJSON, Result(..))
import           Data.Default.Class
import           Data.Foldable (fold)
import           Data.Monoid ( (<>) )

issName, idKey :: T.Text
issName = "pbjdollys.com"
idKey   = "userid"

type TestAPI = "test" :> ReqBody '[OctetStream] TokenStream :> Get '[OctetStream] TokenStream

testEndpoint :: Server TestAPI
testEndpoint = \t -> case t of TokenStream ts -> return $ TokenStream ts
  

type KeyframeAPI = "api" :> (PublicAPI :<|> PrivateAPI)
-- this is a workaround to avoid a missing instance for JS generation in endpoints with OctetStream results and inputs
type APIToJS    = "api" :> PublicAPI' --(PublicAPI' :<|> PrivateAPI)

serverContext :: Context (BasicAuthCheck UserId ': '[])
serverContext = loginCheck :. EmptyContext

keyframeEndpoints :: JWK -> JWSHeader -> Server KeyframeAPI
keyframeEndpoints jwk headers =
  ((uncurry addUser)
   :<|> loginHandler jwk headers)
   :<|> (getAllKeyframes jwk)
   :<|> getKeyframesByID 
   :<|> postKeyframeList

type PublicAPI' =
       "user" :> "new"   :> ReqBody '[JSON] (User, Password)                  :> Post '[JSON] UserId

type PublicAPI =
  PublicAPI'
  :<|> "user" :> "login" :> BasicAuth "login" UserId                          :> Get  '[OctetStream] TokenStream 

type PrivateAPI =
       "all"  :> ReqBody '[OctetStream] TokenStream :> Get '[JSON] [(KeyframeListId, KeyframeList)]       
  :<|> "single" :> Capture "userId" Integer :> Capture "frameListID" Integer :> Get  '[JSON] (Maybe KeyframeList)
  :<|> "new" :> Capture "userId" Integer :> ReqBody '[JSON] KeyframeList     :> Post '[JSON] (Maybe KeyframeListId)
       
-- more instance for documentation


instance ToCapture (Capture "userId" Integer) where
  toCapture _  = DocCapture "userId" "(integer) user id in database"

instance ToCapture (Capture "frameListID" Integer) where
  toCapture _  = DocCapture "frameListID" "(integer) keyframe list id in database"

instance ToAuthInfo (BasicAuth "login" UserId) where
  toAuthInfo _ = DocAuthentication "HTTP BasicAuthentication" "BasicAuthentication: username:password"

keyframeAPI :: Proxy KeyframeAPI
keyframeAPI = Proxy

apiToJS :: Proxy APIToJS
apiToJS = Proxy

-- functions for each endpoint

getAllKeyframes :: JWK -> TokenStream -> ExceptT ServantErr IO [(KeyframeListId, KeyframeList)]
getAllKeyframes jwk  (TokenStream token) = do
  let jwtE = decodeCompact token
  case jwtE of
    Left e -> throwE (err501 { errBody = "Error uncompacting jwt: " <> BS.pack (show e) })
    Right jwt -> do
      validated <- liftIO $ validateJWT jwk jwt
      if validated 
         then do
           let claimHM = _unregisteredClaims. jwtClaimsSet $ jwt
           case HM.lookup idKey claimHM of
             Nothing -> throwE (err501 { errBody = "JWT Claimsset missing userid field" })
             Just jsonUID -> do
               let uId = undefined jsonUID
               frames <- liftIO $ DB.runDB . DB.getAllKeyframeLists . UserEntryKey . fromIntegral $ uId
               return . fmap (\(key, list) -> (KeyframeListId . fromIntegral . unSqlBackendKey . unKeyframeListEntryKey $ key, list)) $ frames
         else throwE (err501 { errBody = "Invalid JWT" })

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

loginHandler :: (MonadIO m) => JWK -> JWSHeader -> UserId -> ExceptT ServantErr m TokenStream
loginHandler jwk header uid = do
  curTime <- liftIO getPOSIXTime
  token <- mkToken jwk header (posixSecondsToUTCTime curTime) uid
  case token of
    Left _      -> throwE err500 
    Right token -> 
      case toCompact token of
        Left _             -> throwE err500
        Right compactToken -> return . TokenStream . fold $ compactToken

validateJWT :: MonadRandom m => JWK -> JWT -> m Bool
validateJWT jwk jwt = do
  return (validateJWSJWT def AnyValidated jwk jwt)

mkToken :: MonadIO m => JWK -> JWSHeader -> UTCTime -> UserId -> m (Either Error JWT)
mkToken secret header curTime userId = liftIO (createJWSJWT secret header claims) where
  claims = emptyClaimsSet {
    _claimIss = Just (fromString issName),
    _claimExp  = (Just $ NumericDate $ addUTCTime (24 * 60 * 60) curTime),
    _unregisteredClaims = HM.fromList [(idKey, toJSON userId)]
    }

getIdFromToken :: JWT -> Result UserId 
getIdFromToken jwt = fromJSON $ (_unregisteredClaims . jwtClaimsSet $ jwt ) HM.! idKey
