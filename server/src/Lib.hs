{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Lib
    ( web
    ) where

import           Control.Lens        hiding (from, (^.))
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Set            as Set
import           Data.Text
import           Data.UUID
import           GHC.Generics
import           Snap
import           Snap.CORS
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           Utils

-- App
data App = App
makeLenses ''App

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

initialHackList :: [Hack]
initialHackList =
  [Hack {hackId = 1
        ,hackName = "ISS SDR"
        ,votes = Set.empty}
  ,Hack {hackId = 2
        ,hackName = "Hack Night Voting System"
        ,votes = Set.empty}
  ,Hack {hackId = 3
        ,hackName = "A*"
        ,votes = Set.empty}]

hackListHandler :: Handler App App [Hack]
hackListHandler =
  method GET $
     return initialHackList

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
     return App

web ::  IO ()
web  =
  do snapConfig <- commandLineConfig . setPort 5000 $ defaultConfig
     serveSnaplet snapConfig initApp
