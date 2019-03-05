import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck as QC
import Math.Function.Trafo.FFT.Triangular
import Linear (V2(..))

main :: IO ()
main = defaultMain $ testGroup "Tests"
 [ testGroup "Triangular-function sampling"
  [ testProperty "xy-Sampling"
     $ \(QC.Positive resx) -> let resy = resx
         in
             homSampledTriangFnData
                (sampleTriangle (V2 resx resy)
                                (\(UnitTriangle (V2 x y)) -> V2 x y)
                            :: HomSampledTriangFunction V2 (V2 Double))
           === [ [V2 x y | x <- xs]
               | y <- (/fromIntegral resy)<$>[0.5,1.5..fromIntegral resy]
               , let xs = [x | x <- (/fromIntegral resx)<$>[0.5,1.5..fromIntegral resx]
                             , x+y<=1 ]
               , not $ null xs ]
  ]
 ]
