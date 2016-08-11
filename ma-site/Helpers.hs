module Helpers where

import Import
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Dictionary as Dict
import Thesis.Data.Stackoverflow.Question

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

linkToAnswer :: DataDictionary IO -> AnswerId -> Widget
linkToAnswer dict aId@(AnswerId{..}) = do
  parentMaybe <- liftIO $ runMaybeT $ Dict.answerParent dict aId
  case parentMaybe of
    Nothing ->
      [whamlet|#{show $ answerIdInt}|]
    Just qId -> let a = show $ answerIdInt
                    q = show $ questionIdInt qId
                    ln = "http://stackoverflow.com/questions/" ++ q ++ "/a/" ++ a
                in [whamlet|<a href=#{ln}>#{a} |]
