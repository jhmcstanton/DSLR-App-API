{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DslrWWW.Types
  (
    Keyframe(..),
    KeyframeList(..),
    Username(..),
    Email(..),
    User(..),
    Password(..),
    UserId(..),
    KeyframeListId(..),
    TokenStream(..)
  ) where

import           Servant
import           Servant.Docs
import           Servant.API.ContentTypes
import           GHC.Generics
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BS (pack) 
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Data.Proxy

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

newtype UserId         = UserId Integer deriving (Generic)

instance FromJSON UserId
instance ToJSON   UserId

newtype KeyframeListId = KeyframeListId Integer deriving (Generic)

instance FromJSON KeyframeListId
instance ToJSON   KeyframeListId

newtype TokenStream    = TokenStream ByteString deriving (Generic)

instance MimeRender OctetStream TokenStream where
  mimeRender p (TokenStream bs) = mimeRender p bs

instance MimeUnrender OctetStream TokenStream where
  mimeUnrender p bs = fmap TokenStream $ mimeUnrender p bs

instance FromHttpApiData TokenStream where
  parseHeader = return . TokenStream . fromStrict


instance ToSample  UserId where
  toSamples _ = singleSample (UserId 5432)

instance ToSample Password where
  toSamples _ = singleSample (Password "UserPassword")

instance ToSample User where
  toSamples _ = singleSample (User (Username "jims_frames") "Jim" "Stanton" (Email "jim@pbjdollys.com"))

instance ToSample TokenStream where
  toSamples _ = singleSample (TokenStream $ BS.pack ("JWT" :: String) )

instance ToSample T.Text where
  toSamples _ = singleSample "Sample Text response"
                             
instance ToSample KeyframeList where
  toSamples _ =
    [ ("Keyframe list with minimum info", emptyCase)
    , ("Small, named keyframe", singleFrame)
    , ("Multiple frames - the DSLR Dolly can generate transitions for this", multipleFrames)
    ]

instance ToSample KeyframeListId where
  toSamples _ = singleSample (KeyframeListId 321)
    
emptyCase      = KeyframeList Nothing []
singleFrame    = KeyframeList (Just "My Starter Keyframe List") [initFrame]
multipleFrames = KeyframeList (Just "A few more frames") [initFrame, Keyframe 15 0 0 30, Keyframe 15 30 30 40]
initFrame      = Keyframe 0 0 0 0
