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

newtype FFTDepth = FFTDepth { fourierDoublings :: Int }

type ℂ = Complex Double

