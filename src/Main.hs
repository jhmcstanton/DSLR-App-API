import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Text
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T

import DslrWWW.Types
import DslrWWW.API

type ServerAPI = Get '[PlainText] Text :<|> KeyframeAPI

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

server :: TVar [KeyframeList] -> Text -> Server ServerAPI
server keyframeLists home =
       return home
  :<|> getAllKeyframes keyframeLists
  :<|> getKeyframesByID keyframeLists
  :<|> postKeyframeList keyframeLists


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Welcome to Haskell on Heroku" T.pack $
                 lookup "TUTORIAL_HOME" env
    emptyKeyframes <- newTVarIO []
    run port $ serve serverAPI $ server emptyKeyframes home
