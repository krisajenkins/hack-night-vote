{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS -fno-warn-orphans #-}
module Lib
    ( web
    ) where

import           Control.Concurrent.STM
import           Control.Lens           hiding (from, (^.))
import           Control.Monad.IO.Class
-- import           Control.Monad.State.Class
-- import           Control.Monad.STM
import           Data.Aeson
import           Data.ByteString        (ByteString)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Set               as Set
import           Data.Text
import           Data.UUID
import           GHC.Generics
import           Snap
import           Snap.CORS
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           Utils

data Hacker =
  Hacker {hackerId   :: UUID
         ,hackerName :: Text}
  deriving (Show,Eq,Generic,ToJSON,FromJSON)

data Hack =
  Hack {hackId   :: Int
       ,hackName :: Text
       ,votes    :: Set UUID}
  deriving (Show,Eq,Generic,ToJSON,FromJSON)

instance FromJSON UUID where
  parseJSON =
    withText "UUID"
             (\t ->
                case fromText t of
                  Nothing -> fail (unpack t)
                  Just uuid -> return uuid)


instance ToJSON UUID where
  toJSON = String . toText

-- App
type Election = (Map UUID Hacker,Map Int Hack)
data App =
  App {_election :: TVar Election}
makeLenses ''App

initialElection :: Election
initialElection =
  (Map.empty
  ,Map.fromList
     [(1
      ,Hack {hackId = 1
            ,hackName = "ISS SDR"
            ,votes = Set.empty})
     ,(2
      ,Hack {hackId = 2
            ,hackName = "Hack Night Voting System"
            ,votes = Set.empty})
     ,(3
      ,Hack {hackId = 3
            ,hackName = "A*"
            ,votes = Set.empty})])

hackListHandler :: Handler App App [Hack]
hackListHandler =
  method GET $
     do electionVar <- view election
        (_,hacks) <- liftIO . atomically $ readTVar electionVar
        return (Map.elems hacks)

routes :: [(ByteString, Handler App App ())]
routes = [("",serveDirectory "static"),("/api",apiRoutes)]
  where apiRoutes =
          applyCORS defaultOptions $
          route [("/hacks",hackListHandler >>= writeJSON)]

initApp ::  SnapletInit App App
initApp  =
  makeSnaplet "newcradle" "New Cradle Webserver" Nothing $
  do
     addRoutes routes
     wrapSite withCompression
     _election <- liftIO . atomically $ newTVar initialElection
     return App {..}

web ::  IO ()
web  =
  do snapConfig <- commandLineConfig . setPort 5000 $ defaultConfig
     serveSnaplet snapConfig initApp
