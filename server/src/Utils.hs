{-# LANGUAGE OverloadedStrings #-}
module Utils where


import           Data.Aeson
import           Data.ByteString
import           Snap

mimeTypeJson, mimeTypeCss :: ByteString
mimeTypeJson = "application/json"
mimeTypeCss = "text/css"

jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setContentType mimeTypeJson


writeJSON :: (ToJSON a,MonadSnap m)
          => a -> m ()
writeJSON a =
  do jsonResponse
     (writeLBS . encode) a
