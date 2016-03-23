module DslrWWW.Types
  (
    Keyframe(..),
    KeyframeList(..),
    Username(..),
    Email(..),
    User(..),
    Password(..)
  ) where

import           GHC.Generics
import           Data.Aeson
import qualified Data.Text as T

data Keyframe = Keyframe {
    position  :: Double,
    panAngle  :: Double,
    tiltAngle :: Double,
    time      :: Double
  } deriving (Generic, Show)

instance FromJSON Keyframe
instance ToJSON   Keyframe

data KeyframeList = KeyframeList {
    name      :: Maybe T.Text,
    keyframes :: [Keyframe]
  } deriving (Generic)

instance FromJSON KeyframeList
instance ToJSON   KeyframeList

newtype Username = Username T.Text deriving (Generic)

instance FromJSON Username
instance ToJSON   Username

newtype Password = Password T.Text deriving (Generic)

instance FromJSON Password
instance ToJSON   Password

newtype Email    = Email    T.Text deriving (Generic)

instance FromJSON Email
instance ToJSON   Email

data User = User {
    username  :: Username,
    firstName :: T.Text,
    lastName  :: T.Text,
    email     :: Email
  } deriving (Generic)

instance FromJSON User
instance ToJSON   User
