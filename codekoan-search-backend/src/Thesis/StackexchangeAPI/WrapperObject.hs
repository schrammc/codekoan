{-# LANGUAGE OverloadedStrings #-}
module Thesis.StackexchangeAPI.WrapperObject where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Vector hiding ((++))

-- | Every valid reply from Stackexchange is delivered in a wrapper object
-- like this.
data WrapperObject =
  WrapperObject { wrapperItems :: Vector Value -- ^ A Vector of JSON objects
                , wrapperPage :: Int
                , wrapperPageSize :: Int
                , wrapperTotal :: Int
                , wrapperHasMore :: Bool
                , wrapperBackoff :: Maybe Int -- ^ Number of seconds we have to
                                              -- wait before the next query of
                                              -- the same type (if defined)
                , wrapperType :: StackoverflowReplyType -- ^ The type of the
                                                        -- delivered content
                }
  | WrapperError { wrapperErrorId :: Maybe Int
                 , wrapperErrorMessage :: Maybe Text
                 , wrapperErrorName :: Maybe Text
                 }
  deriving (Show, Eq)

instance FromJSON WrapperObject where
  parseJSON obj = wrapperParser obj  <|>
                  errorParser obj
    where
      wrapperParser (Object o) = do
        typeString <- o .: "type"
        typ <- case readReplyType typeString of
          Nothing -> fail $ "Unknown type of response: " ++ (show typeString)
          Just x ->  return x
        WrapperObject <$>
          o .: "items" <*>
          o .: "page" <*>
          o .: "page_size" <*>
          o .: "total" <*>
          o .: "has_more" <*>
          o .:? "backoff" <*>
          (pure typ)
      wrapperParser _ = fail "wrapperParser has non-object argument!"
      errorParser (Object o) = WrapperError <$>
                                     o .: "error_id" <*>
                                     o .: "error_message" <*>
                                     o .: "error_name"
      errorParser _ = fail "errorParser has non-object argument!"

-- | The type of content that is stored in a wrapper object.
data StackoverflowReplyType = SOQuestionType
                            | SOAnswerType
                            | SOFilterType
                            | SOCommentType
                            deriving (Eq, Show)


-- | Parse a reply type from a piece of text.
readReplyType :: Text -> Maybe StackoverflowReplyType
readReplyType t = case t of
  "question" -> Just SOQuestionType
  "filter" -> Just SOFilterType
  "comment" -> Just SOCommentType
  "answer" -> Just SOAnswerType
  _ -> Nothing

-- | Write a reply type to text, guarantee
writeReplyType :: StackoverflowReplyType -> Text
writeReplyType SOQuestionType = "question"
writeReplyType SOAnswerType = "answer"
writeReplyType SOFilterType = "filter"
writeReplyType SOCommentType = "comment"

-- | A helper function to parse all item JSON objects to a certain type
wrapperParseItems :: FromJSON a => WrapperObject -> Vector (Maybe a)
wrapperParseItems = (parseMaybe (parseJSON) <$>) . wrapperItems
