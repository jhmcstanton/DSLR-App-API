import           Control.Monad.IO.Class
import           Data.Text
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Docs
import           Servant.JS
import qualified Servant.JS.Angular as NG
import           System.Environment
import           System.IO
import           Database.Persist.Postgresql
import qualified Data.List as L 

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import           DslrWWW.Types
import           DslrWWW.API
import           DslrWWW.Database (runDB, buildDBString)
import           DslrWWW.Database.Models (migrateAll)
import           DslrWWW.Database.Marshal

type ServerAPI = Get '[PlainText] Text :<|> KeyframeAPI

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

server :: Text -> Server ServerAPI
server home =
       return home
  :<|> keyframeEndpoints     

main :: IO ()
main = do          
  hSetBuffering stdout LineBuffering
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
      home = maybe "Welcome to Haskell on Heroku" T.pack $
               lookup "TUTORIAL_HOME" env
  runDB $ runMigration migrateAll
  -- build docs if this is on a dev machine
  dbString <- buildDBString
  if "localhost" `T.isInfixOf` (T.decodeUtf8 dbString)
     then do
       let docsToWrite = markdown $ docs keyframeAPI
       writeFile   "API.md" docsToWrite
       writeJSForAPI keyframeAPI (angularServiceWith (NG.defAngularOptions { NG.serviceName = "backendService"})
                                  (defCommonGeneratorOptions { moduleName = "dslr" })
                                 ) "./angular-client.js"
     else return ()
  putStrLn "Ran migrations"
  run port $ serve serverAPI $ server home
