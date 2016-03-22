{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DslrWWW.Database.Models where

import           DslrWWW.Types

import           Database.Persist.TH
import           Data.Text (Text)


-- newtypes for marshaling to the database, unfortunate but necessary
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserEntry
  t_firstName Text
  t_lastName  Text
  t_username  Text
  t_email     Text
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
|]
