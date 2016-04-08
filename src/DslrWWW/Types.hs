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
    UserSession(..),
    UserAgent(..),
    IPAddress(..),
    toIPAddress
  ) where

import           Servant.Docs
import           Servant.API.ContentTypes
import           GHC.Generics
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Data.Time.Clock.POSIX
import           Data.Int (Int64, Int8)
import           Text.Read(readMaybe)

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

newtype UserAgent      = UserAgent T.Text deriving (Generic)

instance FromJSON UserAgent
instance ToJSON   UserAgent


toIPAddress :: T.Text -> Maybe IPAddress
toIPAddress t = collect (T.unpack t) >>= verify   where
  verify [] = Nothing
  verify ip | length (take 5 ip) == 4 =
              Just $ IPAddress (ip !! 3, ip !! 2, ip !! 1, ip !! 0)
            | otherwise               = Nothing
  collect :: String -> Maybe [Int8]
  collect [] = Just []
  collect s  = (:) <$> 
    (readMaybe $ takeWhile (/= ':') s ) <*>
    collect (drop 1 $ dropWhile (/= ':') s)

newtype IPAddress      = IPAddress (Int8, Int8, Int8, Int8)
                       deriving (Generic, Show)

instance FromJSON IPAddress
instance ToJSON   IPAddress

newtype  UserSession    = UserSession Integer
                      deriving (Generic, Show)

instance FromJSON UserSession
instance ToJSON   UserSession

{-newtype LoginToken = LoginToken ByteString deriving (Generic)

instance FromJSON LoginToken where
 parseJSON (Object v) = LoginToken . T.encodeUtf8 <$> v .: "token"
 parseJSON _          = mzero
  
instance ToJSON   LoginToken where
  toJSON (LoginToken token) = object ["token" .= T.decodeUtf8 token ] -}
-- instances for documentation

instance ToSample  UserId where
  toSamples _ = singleSample (UserId 5432)

instance ToSample Password where
  toSamples _ = singleSample (Password "UserPassword")

instance ToSample User where
  toSamples _ = singleSample (User (Username "jims_frames") "Jim" "Stanton" (Email "jim@pbjdollys.com"))

instance ToSample T.Text where
  toSamples _ = singleSample "Sample Text response"

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

{-instance ToSample LoginToken where
  toSamples _ = singleSample $ LoginToken mempty-}

instance ToSample UserAgent where
  toSamples _ = singleSample
    (UserAgent
     "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1")

instance ToSample IPAddress where
  toSamples _ = singleSample (IPAddress (127, 0, 0, 1))

instance ToSample UserSession where
  toSamples _ = singleSample (UserSession 1231)
