module QuantAnalysis where

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold

-- | Process a stream of financial data
processFinancialData :: Stream.Stream IO Double -> IO Double
processFinancialData stream = Stream.fold Fold.sum stream
