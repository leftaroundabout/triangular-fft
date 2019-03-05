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
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE LambdaCase             #-}

module Math.Function.Trafo.FFT.Triangular
        ( UnitTriangle(..)
        , HomSampledTriangFunction
        , evalTriangFunction
        , sampleTriangle
        -- * Internals
        , homSampledTriangFnData
        ) where

import Data.Manifold.Types
import Data.Complex
import Data.List
import Data.VectorSpace
import qualified Linear as Lin

import qualified Data.Vector.Unboxed as UArr

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State

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

evalTriangFunction :: (Foldable dims, Applicative dims, Lin.Additive dims)
     => HomSampledTriangFunction dims s -> UnitTriangle dims -> s
evalTriangFunction (HomSampledTriangFunction dat resos basept span) (UnitTriangle p)
      = let pInArr = basept Lin.^+^ span Lin.!* p
            linIndex = snd . (`execState`(1,0)) . forM_ (liftA2 (,) pInArr resos)
                         $ \(iInDir,n) -> do
                 (stride, iLin) <- get
                 put (stride * n, iLin + round iInDir*stride)
        in undefined

sampleTriangle :: ( Num s, UArr.Unbox s
                  , Traversable dims, Applicative dims, VectorSpace (dims Double) )
         => dims Int -> (UnitTriangle dims -> s) -> HomSampledTriangFunction dims s
sampleTriangle resos f
    = HomSampledTriangFunction 
        (UArr.generate (foldl' (*) 1 resos)
          $ \iLin -> let poss = evalState (traverse (\rHere->do
                                    iRun <- get
                                    let (iRem, iHere) = iRun`divMod`rHere
                                    put iRem
                                    return $ (fromIntegral iHere + 0.5) / fromIntegral rHere
                                   ) resos) iLin
                     in if foldl' (+) 0 poss <= 1
                         then f $ UnitTriangle poss
                         else 0 )
        resos
        zeroV
        Lin.identity

type family ListNesting (dims :: * -> *) s :: *
type instance ListNesting Lin.V0 s =     s
type instance ListNesting Lin.V1 s =    [s]
type instance ListNesting Lin.V2 s =   [[s]]
type instance ListNesting Lin.V3 s =  [[[s]]]
type instance ListNesting Lin.V4 s = [[[[s]]]]

class HomSampledDataView (dims :: * -> *) where
  homSampledTriangFnData :: UArr.Unbox s
                 => HomSampledTriangFunction dims s -> ListNesting dims s

instance HomSampledDataView Lin.V0 where
  homSampledTriangFnData (HomSampledTriangFunction d _ _ _) = d UArr.! 0
instance HomSampledDataView Lin.V1 where
  homSampledTriangFnData (HomSampledTriangFunction d _ _ _) = UArr.toList d
instance HomSampledDataView Lin.V2 where
  homSampledTriangFnData (HomSampledTriangFunction d resos@(Lin.V2 rx ry) _ _)
     = filter (not . null) $ UArr.ifoldr 
          (\iLin q
             -> let Lin.V2 (ix,x) (iy,y)
                      = evalState (traverse (\rHere->do
                               iRun <- get
                               let (iRem, iHere) = iRun`divMod`rHere
                               put iRem
                               return ( iHere
                                      , (fromIntegral iHere + 0.5) / fromIntegral rHere ) 
                              ) resos) iLin
                in if x+y <= 1
                    then \case (h:l) | ix<rx-1 -> (q:h):l
                               l               -> [q]:l
                    else \case ([]:l) -> []:l
                               l      -> []:l )
          []
          d
