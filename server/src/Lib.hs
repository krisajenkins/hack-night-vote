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

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens                hiding (from, (^.))
import           Control.Monad.IO.Class
import           CreateHacker                (CreateHacker)
import qualified CreateHacker
import           Data.Aeson
import           Data.ByteString             (ByteString)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Set                    as Set
import           Data.Text
import           Data.UUID
import           GHC.Generics
import           Kashmir.Snap.Snaplet.Random
import           Kashmir.Snap.Utils
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
  App {_election              :: TVar Election
      ,_randomNumberGenerator :: Snaplet RandomNumberGenerator}
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

data PublicHack =
  PublicHack {id        :: Int
             ,name      :: Text
             ,voteCount :: Int}
  deriving (Show,Eq,Generic,ToJSON,FromJSON)

fromHack :: Hack -> PublicHack
fromHack hack =
  let id = hackId hack
      name = hackName hack
      voteCount = Set.size ( votes hack)
  in PublicHack {..}


hacksHandler :: Handler App App [PublicHack]
hacksHandler =
  method GET $
  do electionVar <- view election
     (_,hacks) <- liftIO . atomically $ readTVar electionVar
     return (fromHack <$> Map.elems hacks)

hackersHandler :: Handler App App ()
hackersHandler = get <|> post
  where get =
          method GET $
          do electionVar <- view election
             (hackers,_) <- liftIO . atomically $ readTVar electionVar
             writeJSON (Map.elems hackers)
        post =
          method POST $
          do electionVar <- view election
             uuid <- with randomNumberGenerator getRandom
             create :: CreateHacker <- requireBoundedJSON (1024*1024)
             let newHacker =
                   Hacker {hackerId = uuid
                          ,hackerName = CreateHacker.name create }
             liftIO . atomically $
               do (hackers,hacks) <- readTVar electionVar
                  let newHackers = Map.insert uuid newHacker hackers
                  writeTVar electionVar (newHackers,hacks)
             writeJSON newHacker


routes :: [(ByteString, Handler App App ())]
routes = [("",serveDirectory "static"),("/api",apiRoutes)]
  where apiRoutes =
          applyCORS defaultOptions $
          route [("/hacks",hacksHandler >>= writeJSON)
                ,("/hackers",hackersHandler )]

initApp ::  SnapletInit App App
initApp  =
  makeSnaplet "newcradle" "New Cradle Webserver" Nothing $
  do
     addRoutes routes
     wrapSite withCompression
     _randomNumberGenerator <- nestSnaplet "random" randomNumberGenerator initRandom
     _election <- liftIO . atomically $ newTVar initialElection
     return App {..}

web ::  IO ()
web  =
  do snapConfig <- commandLineConfig . setPort 5000 $ defaultConfig
     serveSnaplet snapConfig initApp
