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
import           Data.ByteString
import           Data.Text
import           GHC.Generics
import           Snap
import           Snap.CORS
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           Utils


-- App
data App =
  App {}

makeLenses ''App

data Hack =
  Hack {id   :: Int
       ,name :: Text}
  deriving (Show,Eq,Generic,ToJSON,FromJSON)

hackList :: [Hack]
hackList =
  [Hack {id = 1
        ,name = "ISS SDR"}
  ,Hack {id = 2
        ,name = "Hack Night Voting System"}
  ,Hack {id = 3
        ,name = "A*"}]

hackListHandler :: Handler App App [Hack]
hackListHandler =
  method GET $
     return hackList

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
