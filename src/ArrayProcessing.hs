{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ArrayProcessing where

import Data.Massiv.Array as A
import Data.Massiv.Array.IO as A
import qualified Data.Vector as V
import Pinecone (Vector(..))
import qualified Data.Text as T

-- | Convert financial data from massiv Array to Pinecone Vector
arrayToVector :: (Source r Float) => Array r Ix2 Float -> T.Text -> Vector
arrayToVector arr vecId = 
  let flatVec = V.fromList $ A.toList arr
  in Vector
      { vId = vecId
      , vValues = flatVec
      , vSparseValues = Nothing
      , vMetadata = Nothing
      }

-- | Process a 2D matrix of financial data 
processMatrix :: Array U Ix2 Float -> Array D Ix2 Float
processMatrix matrix = 
  -- Example operations: normalize rows, compute moving averages, etc.
  compute $ A.map (\x -> (x - minVal) / (maxVal - minVal)) matrix
  where
    minVal = minValue matrix
    maxVal = maxValue matrix

-- | Create a matrix from a list of financial time series
createTimeSeriesMatrix :: [[Float]] -> Array U Ix2 Float
createTimeSeriesMatrix rows = 
  fromLists' Seq rows

-- | Extract features from raw financial data
extractFeatures :: Array U Ix2 Float -> Array D Ix2 Float
extractFeatures arr = 
  -- In a real implementation, this would compute various financial indicators:
  -- volatility, momentum, moving averages, etc.
  compute $ A.mapStencil Edge (spatialGradientStencil :: Stencil Ix2 Float Float) arr

-- | Compute time series correlation matrix
correlationMatrix :: Array U Ix2 Float -> Array D Ix2 Float
correlationMatrix matrix = 
  -- Simplified correlation computation
  -- Real implementation would involve proper statistical correlation
  compute $ A.outerWith (*) (A.transpose matrix) matrix

-- | Convert a massiv array to vector format for Pinecone storage
preparePineconeVectors :: Array U Ix2 Float -> [T.Text] -> [Vector]
preparePineconeVectors matrix rowIds =
  zipWith mkVector [0..] rowIds
  where
    mkVector idx vecId = 
      let row = A.computeAs U $ A.slice matrix (Ix1 idx)
      in arrayToVector row vecId
