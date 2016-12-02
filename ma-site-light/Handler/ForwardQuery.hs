module Handler.ForwardQuery where

import Import
import Data.Aeson (encode)
import qualified Thesis.Messaging.Query as Thesis
import Network.HTTP.Simple
postQueryR :: Handler Value
postQueryR = do
  App{..} <- getYesod
  q <- requireJsonBody :: Handler Thesis.Query
  initialRequest <- liftIO $ parseUrl (appSubmitURL appSettings)
  let req = initialRequest { method = "POST"
                           , requestBody = RequestBodyLBS $ encode q
                           }
  response <- httpJSON req
  if statusIsSuccessful $ responseStatus response
    then return $ responseBody response
    else invalidArgs []
