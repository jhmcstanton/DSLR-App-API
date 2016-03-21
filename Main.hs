import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T


newtype Note = Note
    { contents :: Text
    }
  deriving (Generic, Show)

instance FromJSON Note
instance ToJSON Note

data Keyframe = Keyframe {
    position  :: Double,
    panAngle  :: Double,
    tiltAngle :: Double,
    time      :: Double
  }

instance FromJSON Keyframe
instance ToJSON Keyframe

emptyNotes :: IO (TVar [Note])
emptyNotes =
    newTVarIO []

emptyKeyframes = IO (TVar [[Keyframe]])
emptyKeyframes = newTVarIO []

getNotes :: MonadIO m => TVar [Note] -> m [Note]
getNotes notes =
    liftIO $ readTVarIO notes

postNote :: MonadIO m => TVar [Note] -> Note -> m [Note]
postNote notes note =
    liftIO $ do
      T.putStrLn $ contents note
      atomically $ do
        oldNotes <- readTVar notes
        let newNotes = note : oldNotes
        writeTVar notes newNotes
        return newNotes

getKeyframeLists :: MonadIO m => TVar [[Keyframe]] -> m [[Keyframe]]
getKeyframeLists frames =
  liftIO $ readTVarIO frames

postKeyframeList MonadIO m => TVar [[Keyframe]] -> [Keyframe] -> m [[Keyframe]]
postKeyframeList frames newFrames =
  liftIO $ do
    atomically $ do
      oldFrames <- readTVar frames
      let newFrameList = newFrames : oldFrames
      writeTVar frames newFrameList
      return newFrameList

type NoteAPI =
         Get Text
    :<|> "notes" :> Get [Note]
    :<|> "notes" :> ReqBody Note :> Post [Note]

type KeyframeAPI =
  Get Text
  :<|> "keyframes" :> Get [[Keyframe]]
  :<|> "keyframes" :> ReqBody [Keyframe] :> Post [[Keyframe]]

--noteAPI :: Proxy NoteAPI
--noteAPI =
--    Proxy

serverAPI :: Proxy (NoteAPI :<|> KeyframeAPI)
serverAPI = Proxy

server :: Text -> TVar [Note] -> TVar [[Keyframe]] -> Server (NoteAPI :<|> KeyframeAPI) --NoteAPI
server home notes keyframes =
         return home
    :<|> getNotes notes
    :<|> postNote notes
    :<|> getKeyframeLists keyframes
    :<|> postKeyframeList keyframes


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Welcome to Haskell on Heroku" T.pack $
                 lookup "TUTORIAL_HOME" env
    notes <- emptyNotes
    keyframes <- emptyKeyframes
    run port $ serve noteAPI $ server home notes keyframes
