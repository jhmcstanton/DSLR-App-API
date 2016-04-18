{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DslrWWW.API (
    KeyframeAPI,
    apiToJS,
    apiToDocs,
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

import           Control.Lens ( (|>), over )
import           Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import           Servant
import           Servant.Docs
import           Servant.Docs.Internal
import           Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import           Servant.API.Experimental.Auth (AuthProtect)
import           Servant.Server.Experimental.Auth (mkAuthHandler, AuthHandler, AuthServerData)
import           Database.Persist.Sql
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Crypto.JWT
import           Crypto.JOSE.JWS
import           Crypto.JOSE.JWK
import           Crypto.JOSE.Compact 
import           Crypto.JOSE.Error (Error(..))
import           Network.Wai (Request, requestHeaders)
import qualified Data.Text as T (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.HashMap.Strict as HM
import           Data.Time.Clock (addUTCTime, UTCTime)
import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import           Data.Aeson (toJSON, fromJSON, decode, Result(..))
import           Data.Default.Class
import           Data.Foldable (fold)
import           Data.Monoid ( (<>) )

issName, idKey :: T.Text
issName = "pbjdollys.com"
idKey   = "userid"  

type KeyframeAPI = "api" :> (PublicAPI :<|> PrivateAPI)
-- this is a workaround to avoid a missing instance for JS generation in endpoints with OctetStream results and inputs
type APIToJS    = "api" :> PublicAPI' --(PublicAPI' :<|> PrivateAPI)
type APIToDocs  = "api" :> (PublicAPI :<|> PrivateAPIForDocs)

serverContext :: JWK -> Context (BasicAuthCheck UserId ': AuthHandler Request UserId ': '[])
serverContext jwk = loginCheck :. tokenAuth jwk :. EmptyContext

keyframeEndpoints :: JWK -> JWSHeader -> Server KeyframeAPI
keyframeEndpoints jwk headers =
  ((uncurry addUser)
   :<|> loginHandler jwk headers)
   :<|> getAllKeyframes
   :<|> getKeyframesByID 
   :<|> postKeyframeList

type PublicAPI' =
       "user" :> "new"   :> ReqBody '[JSON] (User, Password)                  :> Post '[JSON] UserId

type PublicAPI =
  PublicAPI'
  :<|> "user" :> "login" :> BasicAuth "login" UserId                          :> Get  '[OctetStream] TokenStream 

type TokenAuth = AuthProtect "token-auth"

-- these are added ONLY so that PrivateAPI and PrivateAPIForDocs are basically the same :/
type GetAll     = "all"                                     :> Get '[JSON] [(KeyframeListId, KeyframeList)]
type GetSingle  = "single" :> Capture "frameListID" Integer :> Get  '[JSON] (Maybe KeyframeList)
type AddKF      = "new"    :> ReqBody '[JSON] KeyframeList  :> Post '[JSON] (Maybe KeyframeListId)

-- this is ONLY here since Generalized Authenticaion breaks documentation generation
type PrivateAPIForDocs = GetAll :<|> GetSingle :<|> AddKF

type PrivateAPI = 
       TokenAuth :> GetAll
  :<|> TokenAuth :> GetSingle
  :<|> TokenAuth :> AddKF

type instance AuthServerData (AuthProtect "token-auth") = UserId       
       
-- more instance for documentation


instance ToCapture (Capture "userId" Integer) where
  toCapture _  = DocCapture "userId" "(integer) user id in database"

instance ToCapture (Capture "frameListID" Integer) where
  toCapture _  = DocCapture "frameListID" "(integer) keyframe list id in database"

instance ToAuthInfo (BasicAuth "login" UserId) where
  toAuthInfo _ = DocAuthentication "HTTP BasicAuthentication" "BasicAuthentication: username:password"

instance ToAuthInfo TokenAuth where
  toAuthInfo _ = DocAuthentication "JWT Authentication" "JWT provided by /api/user/login endpoint"

instance (ToAuthInfo (AuthProtect realm), HasDocs sublayout) => HasDocs (AuthProtect realm :> sublayout) where
  docsFor Proxy (endpoint, action) =
    docsFor (Proxy :: Proxy sublayout) (endpoint, action')
      where
        authProxy = Proxy :: Proxy (AuthProtect realm)
        action' = over authInfo (|> toAuthInfo authProxy) action

keyframeAPI :: Proxy KeyframeAPI
keyframeAPI = Proxy

apiToJS :: Proxy APIToJS
apiToJS = Proxy

apiToDocs :: Proxy APIToDocs
apiToDocs = Proxy

-- functions for each endpoint

loginCheck :: BasicAuthCheck UserId
loginCheck = BasicAuthCheck check where
  check (BasicAuthData username password) = do
    maybeUid <- DB.runDB $ DB.checkPassword (Username $ T.decodeUtf8 username) (Password $ T.decodeUtf8 password)
    case maybeUid of
      Nothing    -> return Unauthorized
      (Just uid) -> return . Authorized . UserId . fromIntegral . unSqlBackendKey . unUserEntryKey $ uid

tokenAuth :: JWK -> AuthHandler Request UserId
tokenAuth jwk = mkAuthHandler handler where
  handler req = do
    case lookup "dslr-auth-cookie" (requestHeaders req) of
      Nothing -> throwE (err403 { errBody = "Error in request, unable to find auth cookie" })
      Just token -> do        
        let jwtE = decodeCompact $ fromStrict token
        case jwtE of
          Left e -> throwE (err501 { errBody = "Error uncompacting jwt: " <> BS.pack (show e) })
          Right jwt -> do
            validated <- liftIO $ validateJWT jwk jwt
            if validated 
               then do
                 let claimHM = _unregisteredClaims. jwtClaimsSet $ jwt
                 case HM.lookup idKey claimHM of
                   Nothing -> throwE (err403 { errBody = "JWT Claimsset missing userid field" })
                   Just jsonUID -> case fromJSON jsonUID of
                     Error e     -> throwE (err403 { errBody = "Invalid JWT, unable to decode userid, error: " <> BS.pack (show e) })
                     Success uid -> return uid
               else throwE (err501 { errBody = "Invalid JWT" })

getAllKeyframes :: UserId -> ExceptT ServantErr IO [(KeyframeListId, KeyframeList)]
getAllKeyframes (UserId uId) = do
  frames <- liftIO $ DB.runDB . DB.getAllKeyframeLists . UserEntryKey . fromIntegral $ uId
  return . fmap (\(key, list) -> (KeyframeListId . fromIntegral . unSqlBackendKey . unKeyframeListEntryKey $ key, list)) $ frames

getKeyframesByID :: MonadIO m => UserId -> Integer -> m (Maybe KeyframeList)
getKeyframesByID (UserId uId) kId = liftIO $ do
  let userKey   = UserEntryKey $ fromIntegral uId
  let kfListKey = KeyframeListEntryKey $ fromIntegral kId
  DB.runDB $ DB.getSingleKeyframeList userKey kfListKey

postKeyframeList :: MonadIO m => UserId -> KeyframeList -> m (Maybe KeyframeListId)
postKeyframeList (UserId uId) kfList = liftIO $ do
  let userKey = UserEntryKey $ fromIntegral uId
  kfKey <- DB.runDB $ DB.insertKeyframeList userKey kfList
  return $ fmap (KeyframeListId . fromIntegral . unSqlBackendKey . unKeyframeListEntryKey) kfKey

-- need to check how this could fail
addUser :: MonadIO m => User -> Password -> m UserId
addUser user pw = do
  userKey <- liftIO $ DB.insertUserHashPassword user pw 
  let userId = UserId . fromIntegral . unSqlBackendKey . unUserEntryKey $ userKey
  return userId

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
