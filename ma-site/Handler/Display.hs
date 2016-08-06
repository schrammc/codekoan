{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Display where

import Import

import Control.Monad (foldM_)

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.List as List (intersperse, foldl1, nub)

import Text.Printf
import qualified Data.Text as Text

import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Language.Java (Java, java)

import Thesis.Search
import Thesis.Search.ResultSet
import Thesis.Search.SearchResult

import Thesis.Data.Text.PositionRange
import Thesis.Data.Range 
import Thesis.Data.Stackoverflow.Answer
import Thesis.Data.Stackoverflow.Question
import Thesis.Data.Stackoverflow.Dictionary as Dict
import Helpers
getDisplayR :: Handler Html
getDisplayR = do
  App{..} <- getYesod
  defaultLayout $ do
    [whamlet|
<h1> Result:

<div>
  ^{codeResultWidget appDict codeText undefined}
          |]

codeText :: Text
codeText = "public class Foo{\n    public static void main(String[] args){\n        System.out.println(\"test\");\n    }\n}"

codeResultWidget :: DataDictionary -> Text -> ResultSet t -> Widget
codeResultWidget dict txt results@(ResultSet{..}) = do
  toWidgetHead $ [julius|

         function closeMenus() {
             var dropdowns = document.getElementsByClassName("dropcontent");
             var i;
             for (i = 0; i < dropdowns.length; i++) {
                 var openDropdown = dropdowns[i];
                 if (openDropdown.classList.contains("show")) {
                 openDropdown.classList.remove("show");
                 }
             }
         }
         

         function showDropdown(id){
             closeMenus();
             document.getElementById(id).classList.toggle("show");
         }

         window.onclick = function(event){
             if (!event.target.matches(".dropbtn")) {
                 closeMenus();
             }
         }
         |]
  toWidgetHead [cassius|
.dropcontent
  display: none
  box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2)
  position: absolute;
  z-index: 100;
  background-color: #FFFFFF;

.dropcontent td:hover
  background-color: #f1f1f1


.dropdown
  position: relative
  display: inline-block

.dropbtn
  color: #009900 !important
  font-weight: bold !important
  

.linum
  font-family: Monaco, monospace;
  background-color: rgb(240,240,240);
  border-radius: 4px;

.show
  display: block

.outputclass code
  background-color: rgba(0,0,0,0.0);
  display: inline-block
  margin: -4px;
  color: #000000;

|]
  [whamlet|
  <h2 class="text-center"> Search Results:
  ^{summaryWidget results}
  <div class="outputclass">
    ^{buildOutput dict (Text.replace "\r" "" (Text.replace "\r\n" "\n" txt)) results}


|]

buildOutput :: DataDictionary -> Text -> ResultSet t -> Widget
buildOutput dict txt res = do
  linumWidget 1
  foldM_ f (2 :: Int) codeWith
  where
    codeWith = textMatchedData txt res
    mkLink md = linkToAnswer dict (fragmentMetaAnswerId md)
    linumDigits = length . show . length $ Text.lines txt
    linumWidget n =
      let str = show n
          k = linumDigits - (length str)
          formatted = (concat $ take k (repeat "0")) ++ str
      in [whamlet|<span class="linum">#{formatted}:&nbsp;|]
    f n x = 
      case x of
        (txt, Just mds@(_:_)) -> (menuWidget txt $ mkLink <$> nub mds) >> return n
        (txt, _ ) | txt == "\n" -> do
          [whamlet|<br>|]
          linumWidget n
          return $ n+1
                  | otherwise -> [whamlet|<code>#{txt}|] >> return n


menuWidget :: Text -> [Widget] -> Widget
menuWidget code links = do
  menuId <- newIdent
  [whamlet| <div class="dropdown">
              <code onclick="showDropdown('#{menuId}')" class="dropbtn">#{code}
              <div id="#{menuId}" class="dropcontent">
                <table>
                  <tr>
                    ^{linkW}|]
  where
    linkW = (forM_ links $ \l -> [whamlet|<td> ^{l}|])

textMatchedData :: Text -> ResultSet t -> [(Text, Maybe [AnswerFragmentMetaData])]
textMatchedData t rs = splitSnip $ snipText t $ buildRanges t rs

splitSnip :: [(Text, Maybe a)] -> [(Text, Maybe a)]
splitSnip [] = []
splitSnip ((txt, v):rest) | txt == "" = splitSnip rest
                          | otherwise = lst ++ splitSnip rest
  where
    lst = List.intersperse ("\n",Nothing) ((,v) <$> Text.splitOn "\n" txt )

-- | Replace each range with the piece of text it represents.
snipText :: Text -> [(Range, a)] -> [(Text, Maybe a)]
snipText txt rs = snip txt rs 0
  where
    snip txt [] _ | txt == "" = []
                  | otherwise = [(txt, Nothing)]
    snip txt (r@(Range a b,x):rest) n = 
      if | a == n ->
           (Text.take (b-a) txt, Just x ):(snip (Text.drop (b - a) txt) rest b)
         | a > n  ->
           (Text.take (a-n) txt, Nothing):(snip (Text.drop (a - n) txt) (r:rest) a)
         | otherwise -> error "Unexpected (snipText)"

buildRanges :: Text -> ResultSet t -> [(Range, [AnswerFragmentMetaData])]
buildRanges t rs = do
  range <- rangeSplits resultRanges
  let resultData = do
        (rg, dat) <- resultRangesWithData
        if overlap rg range
          then [dat]
          else []
  return $ (range, resultData)
  where
    results = listOfResults rs
    resultRanges = resultTextRange <$> results
    resultData = resultMetaData <$> results
    resultRangesWithData = zip resultRanges resultData

summaryWidget :: ResultSet t -> Widget
summaryWidget ResultSet{..} =
  [whamlet|<div class="alert alert-success">
            Matches with #{nFragments} fragments of #{nAnswers} answers were found!
          |]
  where
    nAnswers = M.size resultSetMap
    nFragments = M.foldl (\x -> \mp -> x + M.size mp) 0 resultSetMap

toRanges :: Text -> [PositionRange] -> [Range]
toRanges txt rs = do
  PositionRange{..} <- rs
  return $ Range (positionToN posRangeStart) (positionToN posRangeEnd)
  where
    lineV = V.fromList $ length <$> Text.lines txt
    positionToN (Position{..}) =
      (sum (V.take (posLine - 1) lineV)) + (posLine - 1) + (posCol - 1)
  
