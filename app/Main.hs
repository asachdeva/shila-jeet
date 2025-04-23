module Main where

import qualified Streamly.Data.Stream as Stream
import QuantAnalysis (processFinancialData)

main :: IO ()
main = do
  putStrLn "Quant Analysis Lambda Function"
  -- Example data stream (in a real application, this would come from market data)
  let dataStream = Stream.fromList [1.0, 2.0, 3.0, 4.0, 5.0]
  result <- processFinancialData dataStream
  putStrLn $ "Total: " ++ show result
