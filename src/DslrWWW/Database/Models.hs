{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DslrWWW.Database.Models where

import           DslrWWW.Types

import           Database.Persist.TH
import           Data.Text (Text)
import           Data.ByteString (ByteString)


-- newtypes for marshaling to the database, unfortunate but necessary
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserEntry
  t_firstName Text
  t_lastName  Text
  t_username  Text
  UniqueUsername t_username
  t_email     Text
  UniqueEmail t_email
  t_pwHash    ByteString
  deriving Show
KeyframeListEntry
  t_user     UserEntryId
  t_listName Text Maybe
  deriving Show
KeyframeEntry
  t_list      KeyframeListEntryId
  t_position  Double
  t_panAngle  Double
  t_tiltAngle Double
  t_time      Double
  deriving Show
LoginTokenEntry
  t_user      UserEntryId
  UniqueUser  t_user
  t_validUntil Int
  t_token     ByteString
|]
