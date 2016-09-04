import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core

import Control.Concurrent.MVar

main :: IO ()
main = do
  requestCounter <- newMVar 0
  warp 3000 (App requestCounter)
