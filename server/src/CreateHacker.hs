{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CreateHacker where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
data CreateHacker =
  CreateHacker {name :: Text}
  deriving (Show,Eq,Generic,ToJSON,FromJSON)
