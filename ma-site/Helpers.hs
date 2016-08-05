module Helpers where

import Import
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Dictionary as Dict
import Thesis.Data.Stackoverflow.Question

linkToAnswer :: DataDictionary -> AnswerId -> Widget
linkToAnswer dict aId@(AnswerId{..}) = 
  case Dict.answerParent dict aId  of
    Nothing ->
      [whamlet|#{show $ answerIdInt}|]
    Just qId -> let a = show $ answerIdInt
                    q = show $ questionIdInt qId
                    ln = "http://stackoverflow.com/questions/" ++ q ++ "/a/" ++ a
                in [whamlet|<a href=#{ln}>#{a} |]
