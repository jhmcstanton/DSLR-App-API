module DslrWWW.Types ( Keyframe(..), KeyframeList) where

import GHC.Generics
import Data.Aeson

data Keyframe = Keyframe {
    position  :: Double,
    panAngle  :: Double,
    tiltAngle :: Double,
    time      :: Double
  } deriving (Generic, Show)

instance FromJSON Keyframe
instance ToJSON   Keyframe

type KeyframeList = [Keyframe]
