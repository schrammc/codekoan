{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc
    ) where

import           Data.Binary

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource

import           Control.Concurrent.MVar

import           Data.Attoparsec.Text as AP

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import           Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import           Data.List (foldl')

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import           Data.Vector (Vector)
import           Data.Vector.Binary
import qualified Data.Vector as V

import GHC.Generics (Generic)

import           Data.XML.Types (Event)
import           Text.XML.Stream.Parse

import           Thesis.CodeAnalysis.StackoverflowBodyParser
import           Thesis.Tokenizer
import           Thesis.Trie
import           Thesis.Levenstein

import System.IO

import Data.Monoid

parseTags :: Parser (Vector Text)
parseTags = V.fromList <$> AP.many' tag
  where
    tag = do
      char '<'
      content <- AP.takeWhile (/= '>')
      char '>'
      return content

data StackoverflowPost = PostQuestion !Question
                       | PostAnswer !Answer
                       | PostUnknown
                       deriving (Show, Eq)

isQuestion :: StackoverflowPost -> Bool
isQuestion (PostQuestion _) = True
isQuestion _ = False

isAnswer :: StackoverflowPost -> Bool
isAnswer (PostAnswer _) = True
isAnswer _ = False

questionOrAnswer :: StackoverflowPost -> Bool
questionOrAnswer PostUnknown = False
questionOrAnswer _           = True

codeLength (PostQuestion Question{..}) =
  case readCodeFromHTMLPost questionBody of
    [] -> Nothing
    _ -> Just $ (foldl1 max) $ Text.length <$> readCodeFromHTMLPost questionBody
codeLength (PostAnswer Answer{..}) =
  case readCodeFromHTMLPost answerBody of
    [] -> Nothing
    _ -> Just $ (foldl1 max) $ Text.length <$> readCodeFromHTMLPost answerBody
codeLength _ = Nothing

rating (PostQuestion Question{..}) = Just questionRating
rating (PostAnswer Answer{..}) = Just answerRating
rating _ = Nothing

-- | For a post that has code, this function yields the post's rating and the
-- length of it's longest code fragment
ratingAndLength :: StackoverflowPost -> Maybe (Integer, Integer)
ratingAndLength p = do
  l <- codeLength p
  r <- rating p
  return (r,fromIntegral l)

lengthAndRating = (,) <$> codeLength <*> rating

data PostType = AnswerType | QuestionType | OtherType
              deriving (Show, Eq)

data Question = Question { questionId :: !QuestionId
                         , questionBody :: !Text
                         , questionTitle :: !Text
                         , questionRating :: !Integer
                         , questionTags :: !(V.Vector Text)
                         }
              deriving (Show, Eq)
                       
newtype QuestionId = QuestionId {questionIdInt :: Int}
                   deriving (Show, Eq)

data Answer = Answer { answerId :: !AnswerId
                     , answerBody :: !Text
                     , answerRating :: !Integer
                     , answerParent :: !Int
                     }
            deriving (Show, Eq)

newtype AnswerId = AnswerId {answerIdInt :: Int}
                 deriving (Show, Eq)

readPostType "1" = QuestionType
readPostType "2" = AnswerType
readPostType _ = OtherType

parsePost :: ConduitM Event o (ResourceT IO) (Maybe StackoverflowPost)
parsePost = tagName "row" parseAttributes $ return
  where
    parseAttributes = do
      rowId <- readAttribute "Id"
      postType <- readPostType <$> requireAttr "PostTypeId"
      postBody <- requireAttr "Body"
      content <- case postType of
        AnswerType -> do
          score <- readAttribute "Score"
          parent <- readAttribute "ParentId"
          return $ PostAnswer (Answer (AnswerId rowId) postBody score parent)
        QuestionType -> do
          questionTitle <- requireAttr "Title"
          score <- readAttribute "Score"
          tags <- parseWithDefault V.empty parseTags <$> requireAttr "Tags"
          return $ PostQuestion (Question (QuestionId rowId)
                                           postBody
                                           questionTitle
                                           score
                                           tags
                                )
        OtherType -> return PostUnknown
      ignoreAttrs
      return content
    readAttribute x = (read . Text.unpack) <$> requireAttr x

parseWithDefault :: a -> Parser a -> Text -> a
parseWithDefault def p t = (either (const def) (id)) $ parseOnly p t

parsePosts :: ConduitM Event StackoverflowPost (ResourceT IO) ()
parsePosts = void $ tagNoAttr "posts" $ manyYield parsePost

xmlFilePath :: String
--xmlFilePath = "/home/kryo/posts_abridged.xml"
xmlFilePath = "/home/kryo/stackoverflow_dump/Posts.xml"

outfile = "/home/kryo/out"

questionStatfile = "/home/kryo/question_stats"
answerStatfile = "/home/kryo/answer_stats"

writePost (PostQuestion Question{..}) =
  mconcat [ "{\"type\":\"question\""
          , ",\"id\":" <> (Text.pack $ show $ questionIdInt questionId)
          , ",\"code\":" <> (Text.pack . show $ readCodeFromHTMLPost questionBody)
          , ",\"tags\":" <> (Text.pack $ show questionTags)
          , "}"
          ]
writePost (PostAnswer Answer{..}) =
    mconcat [ "{\"type\":\"answer\""
            , ",\"id\":" <> (Text.pack $ show $ answerIdInt answerId)
            , ",\"code\":" <> (Text.pack . show $ readCodeFromHTMLPost answerBody)
            , "}"
            ]

data DataDictionary = DataDictionary { dictAnswerParents :: !(IntMap Int)
                                     , dictQuestionTags :: !(IntMap (S.Set Int))
                                     , dictTagTable :: !(IntMap Text)
                                     }
                      deriving Generic

-- | Get the text tags for an answer's parent question
answerRootTags :: DataDictionary -> AnswerId -> Maybe (S.Set Text)
answerRootTags (DataDictionary{..}) (AnswerId aid) = do
  parent <- IM.lookup aid dictAnswerParents
  tags <- IM.lookup parent dictQuestionTags
  tagStrings <- mapM (\tag -> IM.lookup tag dictTagTable) (S.toList tags)
  return $ S.fromList tagStrings

instance Binary DataDictionary

someFunc :: IO ()
someFunc = do
  answerRootsVar <- newMVar IM.empty :: IO (MVar (IntMap Int))
  questionTagsVar <- newMVar IM.empty
  tagsMapVar <- newMVar (M.empty, 0)
  counterVar <- newMVar (0 :: Integer)

  runResourceT $ do
    (_, handle) <- allocate (openFile outfile WriteMode)
                            (hFlush >> hClose)
    (_, qStatH) <- allocate (openFile questionStatfile WriteMode)
                            (hFlush >> hClose)
    (_, aStatH) <- allocate (openFile answerStatfile WriteMode)
                            (hFlush >> hClose)
                   
    parseFile def xmlFilePath $$ parsePosts
      =$= CL.filter questionOrAnswer
      =$= writeAnswerRoots answerRootsVar
      =$= writeQuestionTags tagsMapVar questionTagsVar
      =$ (awaitForever (\_ -> return ()))

  roots <- readMVar answerRootsVar
  qtags <- readMVar questionTagsVar
  (tagsMap, _) <- readMVar tagsMapVar
  let swap (a,b) = (b,a)
      revTagsMap = IM.fromList (swap <$> M.toList tagsMap)
      dict = DataDictionary roots qtags revTagsMap

  encodeFile "/home/kryo/data_dictionary" dict

  where
    savePost h p =
      case p of
        PostUnknown -> return ()
        _ -> TextIO.hPutStrLn h $ writePost p

    saveStat h s = do
      hPutStrLn h $ (show $ fst s) ++ " " ++ (show $ snd s)

tagValue :: MVar (M.Map Text Int, Int) -> Text -> IO Int
tagValue mpVar t = do
  (mp, n) <- takeMVar mpVar
  case M.lookup t mp of
    Just x  -> putMVar mpVar (mp, n) >> return x
    Nothing -> putMVar mpVar (M.insert t n mp, n+1) >> return n

writeQuestionTags :: (MonadTrans t, Monad (t IO))
                     => MVar (M.Map Text Int, Int)
                     -> MVar (IntMap (S.Set Int))
                     -> Conduit StackoverflowPost (t IO) StackoverflowPost
writeQuestionTags tagV v = CL.iterM $ \p ->
  case p of
    (PostQuestion Question{..}) -> lift $ do
      qInts <- V.mapM (tagValue tagV) questionTags
      let qIntsSet = S.fromList $ V.toList qInts
      modifyMVar_ v $ \mp ->
        return $! IM.insert (questionIdInt questionId) qIntsSet mp
    _ -> return ()
writeAnswerRoots :: (MonadTrans t, Monad (t IO))
                    => MVar (IntMap Int)
                    -> Conduit StackoverflowPost (t IO) StackoverflowPost
writeAnswerRoots v = CL.iterM $ \p ->
  case p of
    (PostAnswer !Answer{..}) -> lift $ modifyMVar_ v $ \mp ->
      return $! IM.insert (answerIdInt answerId) answerParent mp
    _ -> return ()

filterQuestions :: Monad m => Conduit StackoverflowPost m Question
filterQuestions =  CL.filter isQuestion =$= CL.map (\(PostQuestion q) -> q)

filterAnswers :: Monad m => Conduit StackoverflowPost m Answer
filterAnswers =  CL.filter isQuestion =$= CL.map (\(PostAnswer a) -> a)


filterTags :: Monad m => S.Set Text -> Conduit Question m Question
filterTags tgs = CL.filter $ \Question{..} ->
                               not . null $ filter (\t -> S.member t tgs)
                                                   (V.toList questionTags)
