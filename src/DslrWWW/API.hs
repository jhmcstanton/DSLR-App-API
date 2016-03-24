{-# LANGUAGE FlexibleInstances #-}
module DslrWWW.API (
    KeyframeAPI,
    getAllKeyframes,
    getKeyframesByID,
    postKeyframeList,
    addUser
  ) where

import           DslrWWW.Types
import qualified DslrWWW.Database as DB
import           DslrWWW.Database.Models

import           Servant
import           Servant.Docs
import           Database.Persist.Sql
import           Control.Monad.IO.Class
import           Control.Monad.Except
import qualified Data.Text as T (Text)

type KeyframeAPI =
       "api" :> "all" :> Capture "userId" Integer                                     :> Get  '[JSON] [(KeyframeListId, KeyframeList)]
  :<|> "api" :> "user" :> "new" :> ReqBody '[JSON] (User, Password)                   :> Post '[JSON] (Maybe UserId)
  :<|> "api" :> "single" :> Capture "userId" Integer :> Capture "frameListID" Integer :> Get  '[JSON] (Maybe KeyframeList)
  :<|> "api" :> "new" :> Capture "userId" Integer :> ReqBody '[JSON] KeyframeList     :> Post '[JSON] (Maybe KeyframeListId)

-- instances for documentation

instance ToCapture (Capture "userId" Integer) where
  toCapture _ = DocCapture "userId" "(integer) user id in database"

instance ToCapture (Capture "frameListID" Integer) where
  toCapture _ = DocCapture "frameListID" "(integer) keyframe list id in database"

instance ToSample (Maybe UserId) where
  toSamples _ = singleSample (Just (UserId 5432))

instance ToSample Password where
  toSamples _ = singleSample (Password "UserPassword")

instance ToSample User where
  toSamples _ = singleSample (User (Username "jims_frames") "Jim" "Stanton" (Email "jim@pbjdollys.com"))

instance ToSample T.Text where
  toSamples _ = singleSample "ignore this"

instance ToSample KeyframeList where
  toSamples _ =
    [ ("Keyframe list with minimum info", emptyCase)
    , ("Small, named keyframe", singleFrame)
    , ("Multiple frames - the DSLR Dolly can generate transitions for this", multipleFrames)
    ]


    
emptyCase      = KeyframeList Nothing []
singleFrame    = KeyframeList (Just "My Starter Keyframe List") [initFrame]
multipleFrames = KeyframeList (Just "A few more frames") [initFrame, Keyframe 15 0 0 30, Keyframe 15 30 30 40]
initFrame      = Keyframe 0 0 0 0

instance ToSample KeyframeListId where
  toSamples _ = singleSample (KeyframeListId 321)

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
addUser :: MonadIO m => User -> Password -> m (Maybe UserId)
addUser user pw = do
  userKey <- liftIO $ DB.insertUserHashPassword user pw 
  return . Just . UserId . fromIntegral . unSqlBackendKey . unUserEntryKey $ userKey
