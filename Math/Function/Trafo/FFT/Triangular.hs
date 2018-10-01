-- |
-- Module      : Math.Function.Trafo.FFT.Triangular
-- Copyright   : (c) Justus Sagemüller 2018
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}

module Math.Function.Trafo.FFT.Triangular
           where

import Data.Manifold.Types
import Data.Complex
import Data.VectorSpace
import qualified Linear as Lin

import qualified Data.Vector.Unboxed as UArr

import Control.Monad

newtype UnitTriangle dims
    = UnitTriangle { getUnitTrianglePoint :: dims Double
                       -- ^ Coordinates 𝑥ᵢ ≥ 0 with ∑ 𝑥ᵢ ≤ 1
                   }

data HomSampledTriangFunction dims s = HomSampledTriangFunction
  { hstfDataArray :: UArr.Vector s
  , hstfDataResolutions :: dims Int
  , hstfBasepoint :: dims Double
  , hstfSpan :: dims (dims Double)
  }

evalTriangFunction :: (Foldable dims, Lin.Additive dims)
     => HomSampledTriangFunction dims s -> UnitTriangle dims -> s
evalTriangFunction (HomSampledTriangFunction dat resos basept span) (UnitTriangle p)
      = let pInArr = basept Lin.^+^ span Lin.!* p
        in undefined

sampleTriangle :: (Num s, Traversable dims, Applicative dims, VectorSpace (dims Double))
         => dims Int -> (UnitTriangle dims -> s) -> HomSampledTriangFunction dims s
sampleTriangle resos f
    = HomSampledTriangFunction 
        undefined
        resos
        zeroV
        Lin.identity
