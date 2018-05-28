module ComplexUtils (toComplex, toComplexM) where

import Data.Complex (Complex((:+)))


toComplex :: Double -> (Complex Double)
toComplex a = (a :+ 0)


toComplexM :: [Double] -> [(Complex Double)]
toComplexM = map toComplex
