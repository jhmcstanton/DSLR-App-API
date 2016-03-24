module DslrWWW.API (
    KeyframeAPI,
    getAllKeyframes,
    getKeyframesByID,
    postKeyframeList,
  ) where

import           DslrWWW.Types
import qualified DslrWWW.Database as DB
import           DslrWWW.Database.Models

import           Servant
import           Database.Persist.Sql
import           Control.Monad.IO.Class
import           Control.Monad.Except

type KeyframeAPI =
       "api" :> "all" :> Capture "userId" Integer                                 :> Get  '[JSON] [(KeyframeListId, KeyframeList)]
--  :<|> "api" :> "user" :> "new" :> Post '[JSON] (Maybe UserId)
  :<|> "api" :> Capture "userId" Integer :> Capture "frameListID" Integer         :> Get  '[JSON] (Maybe KeyframeList)
  :<|> "api" :> "new" :> Capture "userId" Integer :> ReqBody '[JSON] KeyframeList :> Post '[JSON] (Maybe KeyframeListId)

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
