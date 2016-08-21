module Thesis.CodeAnalysis.Semantic where

import Data.Text (Text)

-- | An interface to a semantic analysis method
data SemanticAnalyzer a =
  SemanticAnalyzer { semanticPreprocess :: [Text] -> a
                   , semanticSimilarity :: a -> a -> Double
                     -- ^ This function must return double values on a scale of
                     -- 0.0 to 1.0. 0.0 is maximally dissimilar while 1.0 is
                     -- either identical or highly similar.
                   }
