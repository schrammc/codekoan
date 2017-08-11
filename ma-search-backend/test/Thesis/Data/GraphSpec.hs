module Thesis.Data.GraphSpec where 

import           Data.List (sort)
import qualified Data.Vector as V
import           Test.Hspec
import           Thesis.Data.Graph

spec :: SpecWith ()
spec = do
  describe "Thesis.Data.GraphSpec" $ do
    -- Example from the wikipedia page on the bron-krebosch algorithm
    it "Graph cliques are correct in a simple example" $ do
      let grx = buildGraphUnsafe (V.fromList [0..5]) (sort $ [(0,4),(0,1),(4,1),(4,3),(1,2), (3,2),(5,3)])
      cliques grx == [[0,1,4],[1,2],[2,3],[3,4],[3,5]]

