module Handler.DisplayAlt where

import Control.Monad.Logger

import           Data.Char (isDigit)
import           Control.Monad.Trans.Maybe
import qualified Data.Text as Text
import           Helper
import           Import
import           Prelude (read)
import qualified Prelude as Prelude
import           Thesis.Data.Range
import           Thesis.Data.Stackoverflow.Answer
import           Thesis.Data.Stackoverflow.Question
import           Thesis.Data.Stackoverflow.Dictionary as Dict
import           Thesis.Messaging.Query
import           Thesis.Messaging.ResultSet
import Text.Julius (RawJS (..))
import Thesis.Util.LoggingUtils

getDisplayAltR :: Int -> Handler Html
getDisplayAltR qInt = do
  App{..} <- getYesod
  reply <- waitForReply appSettings queryId
  case reply of
    Right resultSet -> 
      defaultLayout $ do
        setTitle "CodeK\333an"
        $(widgetFile "display")
    Left errorMsg -> do
      defaultLayout $ [whamlet|
<h1>There was an error in the search backend!

Details: #{errorMsg} <br>
Query identifier: #{qInt}
|]
  where
    queryId = QueryId qInt

resultsW :: ResultSetMsg -> Widget
resultsW resultSet@ResultSetMsg{..} = do
  App{..} <- getYesod
  if null resultSetResultList
    then [whamlet|
                 <center><h1>Your Search Yielded no Results
                 <br style="margin-bottom:10em">
                 |]
    else do
      let groups = resultGroups resultSet
      [whamlet|<h1 style="text-align:center">Code pattern reuse detected at #{length groups} sites:
              |]
      let sortedGroups = reverse $ sortOn fst (rankGroups groups)
      forM_ sortedGroups $ \(rank, gr) ->
        [whamlet|
                <div .row>
                  <div .col-lg-12>
$#                    <b>#{rank}
                    ^{resultGroupW gr}
                |]
  where
    reuses :: Text
    reuses = if length resultSetResultList > 1
             then "reuses"
             else "reuse"

    rankGroups :: [ResultGroup] -> [(Double, ResultGroup)]
    rankGroups gs =
      let maxLength = Prelude.maximum $ groupAvgPatternTokenRangeLength <$> gs
          rankGroup g =
            let l = groupAvgPatternTokenRangeLength g / maxLength
                c = groupAvgPatternCoverage g
            in (c * l, g)
      in rankGroup <$> gs

data ResultGroup = ResultGroup [Range Text] [ResultMsg] Text

groupAvgPatternCoverage :: ResultGroup -> Double
groupAvgPatternCoverage (ResultGroup _ msgs _) =
  (sum $ resultCoverage <$> msgs) / (fromIntegral $ length msgs)

groupAvgPatternTokenRangeLength :: ResultGroup -> Double
groupAvgPatternTokenRangeLength (ResultGroup _ msgs _) =
  (fromIntegral . sum $ totalTokenLength <$> msgs) / (fromIntegral $ length msgs)
  where
    totalTokenLength msg = sum $ do
      match <- resultAlignmentMatches msg
      return . rangeLength $ alignmentMatchPatternTokenRange match

resultGroups :: ResultSetMsg -> [ResultGroup]
resultGroups msg = do
  gr <- groupBy (\a b -> resultDocRanges a == resultDocRanges b) $
                sortOn resultDocRanges (resultSetResultList msg)
  return $ ResultGroup (resultDocRanges $ Prelude.head gr)
                       gr
                       (resultQueryText msg)
  where
    resultDocRanges resMsg =
      sort $ alignmentMatchQueryTextRange <$> resultAlignmentMatches resMsg

resultCoverage :: ResultMsg -> Double
resultCoverage ResultMsg{..} =
  coveragePercentage (Text.length resultFragmentText) ranges
  where
    ranges = alignmentMatchPatternTextRange <$>resultAlignmentMatches

resultGroupW :: ResultGroup -> Widget
resultGroupW gr@(ResultGroup ranges msgs queryText) = do
  let frac = groupAvgPatternCoverage gr
  [whamlet|
           <h3> <b>Site with reuse </b>

           Similarities with #{length msgs} Stack Overflow code

           <div .row>

           <div .row>
             <div .col-lg-6>
               <br>
               <center> The matched #{pieces} from your query document:
               <br>
               ^{queryPieces}
             <div .col-lg-6>
               <br>
               <center> The relevant Stack Overflow code patterns:
               <br>
               ^{fragGroups}
          <hr>
          |]
  where
    frag :: Text
    frag | length msgs == 1 = "fragment"
         | otherwise = "fragments"
    pieces :: Text
    pieces = if length ranges > 1
             then "pieces"
             else "piece"
    queryPieces = forM_ (resultAlignmentMatches $ Prelude.head msgs) $ \AlignmentMatchMsg{..} -> do
      let qt = textInRangeNice alignmentMatchQueryTextRange queryText
      [whamlet|
              <div .well>
                <pre>

                    #{qt}
              |]
    fragGroups = do
      let (firstFrags, restFrags) = Prelude.splitAt 3 msgs
      forM_ firstFrags renderFrag
      when (not $ null restFrags) $ do
        ident <- newIdent
        identBtn <- newIdent
        [whamlet|
<button .btn .btn-info id="#{identBtn}" data-toggle="collapse" data-target="##{ident}" onclick="$('##{identBtn}').hide()">... and #{length restFrags} others

<div id="#{ident}" .collapse>
  ^{forM_ restFrags renderFrag}
|]

    renderFrag ResultMsg{..} = do
      App{..} <- getYesod
      let (aIdTxt, rest) = Text.span isDigit resultSource
          aIdInt = read $ Text.unpack aIdTxt
          aId = AnswerId aIdInt
          link = "http://stackoverflow.com/a/" <> (Text.takeWhile isDigit resultSource)
      Just (parentQuestion, answer) <- liftIO $ runOutstreamLogging $ runMaybeT $ do
        parentId <- Dict.answerParent appDict aId
        q <- Dict.getQuestion appDict parentId
        a <- Dict.getAnswer appDict aId
        return (q, a)
  
      [whamlet|
                <b><a target="_blank" href=#{link}>#{questionTitle parentQuestion}</a>
                <br>
                <br>

                <div .well>
                  <pre>
                    #{resultFragmentText}|]
      return ()

singleResultW :: ResultSetMsg -> ResultMsg -> Widget
singleResultW ResultSetMsg{..} ResultMsg{..} = do
  App{..} <- getYesod
  Just (parentQuestion, answer) <- liftIO $ runOutstreamLogging $ runMaybeT $ do
    parentId <- Dict.answerParent appDict aId
    q <- Dict.getQuestion appDict parentId
    a <- Dict.getAnswer appDict aId
    return (q,a)

  [whamlet|
          <div .row>
            <div .col-lg-12>
              <div .row>
                <div .col-lg-12>
                  <h3><a target="_blank" href=#{link}>#{questionTitle parentQuestion}</a><br>
                  <p>
                    The source answer is rated rating: #{answerRating answer}<br>
                    The answer parent's rating: #{questionRating parentQuestion}
                  <button type="button" class="btn btn-default" onclick="showDialog('#{resultSource}')">
                    <span class="glyphicon glyphicon-thumbs-up"></span>
                    Give Feedback!
              <div .row>
                <div .col-lg-12>

              <div .row>
                <div .col-lg-6>
                  The matched #{pieces} from your query document:
                  <br>
                  <br>
                  ^{queryPieces}
                <div .col-lg-6>
                  The matched code fragment:
                  <br>
                  <br>
                  <div .well style="overflow:scroll">
                    <pre>
                      
                        #{resultFragmentText}
          <hr>
          |]
  where
    link = "http://stackoverflow.com/a/" <> (Text.takeWhile isDigit resultSource)
    queryPieces = forM_ resultAlignmentMatches $ \AlignmentMatchMsg{..} -> do
      let qt = textInRangeNice alignmentMatchPatternTextRange resultQueryText
      [whamlet|
              <div .well>
                <pre>

                    #{qt}
              |]
    pieces :: Text
    pieces = if length resultAlignmentMatches > 1
             then "pieces"
             else "piece"

    (aIdTxt, rest) = Text.span isDigit resultSource
    aIdInt = read $ Text.unpack aIdTxt
    fragIdInt = read $ Text.unpack $ Text.takeWhile isDigit $ Text.tail rest :: Int
    aId = AnswerId aIdInt

textInRangeNice :: Range Text -> Text -> Text
textInRangeNice range text =
  let (txt, (start,end)) = textLinesInRange range text
      txtLines = zip formattedLineNumbers (Text.lines txt)
      formattedLineNumbers :: [Text]
      formattedLineNumbers =
        let nrs = take (end - start) $ Text.pack . show <$> [start ..]
            maxLength = Prelude.maximum $ (Text.length <$> nrs :: [Int])
        in fillTo maxLength <$> nrs
      
      txt' = Text.unlines $ do
        (nr, line) <- txtLines
        return $ nr <> line
  in txt'

fillTo :: Int -> Text -> Text
fillTo n t | Text.length t >= n = t <> ":"
           | otherwise =
               (Text.pack $ take (n - (Text.length t)) (repeat ' ')) <> t <> ":"
