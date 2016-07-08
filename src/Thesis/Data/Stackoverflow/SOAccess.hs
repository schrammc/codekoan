--------------------------------------------------------------------------------
-- |
-- A data structure for sets of functions to access stackoverflow
module Thesis.Data.Stackoverflow.SOAccess where

import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question

data SOAccess = SOAccess { getSOAnswer :: AnswerId -> IO (Maybe Answer)
                         , getSOQuestion :: QuestionId -> IO (Maybe Question)
                         , getSOAnswersTo :: QuestionId -> IO (Maybe [AnswerId])
                         }
