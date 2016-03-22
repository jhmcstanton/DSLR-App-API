module DslrWWW.API where

import DslrWWW.Types

import           Servant
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Concurrent.STM

type KeyframeAPI =
       "api" :> "all"                                 :> Get  '[JSON] [KeyframeList]
  :<|> "api" :> Capture "frameListID" Integer         :> Get  '[JSON]  KeyframeList
  :<|> "api" :> "new" :> ReqBody '[JSON] KeyframeList :> Post '[JSON] [KeyframeList]

keyframeAPI :: Proxy KeyframeAPI
keyframeAPI = Proxy

-- functions for each endpoint
getAllKeyframes  :: MonadIO m => TVar [KeyframeList] -> m [KeyframeList]
getAllKeyframes = liftIO . readTVarIO

getKeyframesByID :: MonadIO m => TVar [KeyframeList] -> Integer -> m KeyframeList
getKeyframesByID keyframes id = liftIO $ do
  atomically $ do
    frameLists <- readTVar keyframes
    return $ frameLists !! (fromIntegral id)

postKeyframeList :: MonadIO m => TVar [KeyframeList] -> KeyframeList -> m [KeyframeList]
postKeyframeList keyframes newKeyframes = liftIO $ do
  atomically $ do
    frameLists <- readTVar keyframes
    let frameLists' = frameLists ++ [newKeyframes]
    writeTVar keyframes frameLists'
    return frameLists'
    
