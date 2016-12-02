module Handler.GetForward where

import Import
import Data.Aeson (encode)
import qualified Thesis.Messaging.Query as Thesis
import Network.HTTP.Simple

getGetForwardR :: Int -> Handler Value
getGetForwardR qInt = do
  App{..} <- getYesod
  initialRequest <- liftIO $ parseUrl (appReplyCacheURL appSettings ++ "/" ++ show qInt)
  let req = initialRequest { method = "GET" }
  response <- httpJSON req
  if statusIsSuccessful $ responseStatus response
    then return $ responseBody response
    else invalidArgs []
