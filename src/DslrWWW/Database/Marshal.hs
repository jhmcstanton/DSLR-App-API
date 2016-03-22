module DslrWWW.Database.Marshal where

import           DslrWWW.Database.Models
import           DslrWWW.Types


entryToUser :: UserEntry -> User
entryToUser (UserEntry t_first t_last t_email t_username) =
  User { firstName = t_first,
         lastName  = t_last,
         email     = Email t_email,
         username  = Username t_username
       }

entryToKeyframe :: KeyframeEntry -> Keyframe
entryToKeyframe (KeyframeEntry _ t_pos t_pan t_tilt t_time) =
  Keyframe { position  = t_pos,
             panAngle  = t_pan,
             tiltAngle = t_tilt,
             time      = t_time
           }

entryToKfList :: KeyframeListEntry -> [KeyframeEntry] -> KeyframeList
entryToKfList (KeyframeListEntry _ t_listName) kfs =
  KeyframeList { name      = id t_listName,
                 keyframes = fmap entryToKeyframe kfs
               }
