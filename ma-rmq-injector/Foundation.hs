{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core
import Settings
import Control.Concurrent.MVar

data App = App { appRequestCounter :: MVar Int}

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App
