module DiscreteFourier (discreteFourier, inverseDiscreteFourier) where

import Data.Complex (Complex, cis, magnitude)
import Data.List (zipWith, length)

import ComplexUtils (toComplex, toComplexM)


getAngles :: Double -> Int -> [(Complex Double)]
getAngles pi2k n = map cis $ map (pi2k *) $ map fromIntegral [0..n-1]


discreteFourierStep :: [(Complex Double)] -> Int -> Int -> (Complex Double)
discreteFourierStep fs op k =
    let
        n = length fs
    in
        if k == 0
            then (foldl (+) (toComplex 0) fs)
            else
                let
                    pi2k = (2 * pi * (fromIntegral k) * (fromIntegral op)) / (fromIntegral n)
                    angles = getAngles pi2k n
                in
                    sum $ zipWith (*) fs angles


iterateDiscreteFourierStep :: Int -> [(Complex Double)] -> [(Complex Double)]
iterateDiscreteFourierStep op fs =
    let
        n = length fs
    in
        map (discreteFourierStep fs op) [0..n-1]


discreteFourier :: [Double] -> [(Complex Double)]
discreteFourier fs = iterateDiscreteFourierStep (-1) $ toComplexM fs


inverseDiscreteFourier :: [(Complex Double)] -> [Double]
inverseDiscreteFourier cs =
    let
        n = length cs
        nd = fromIntegral n
    in
        map magnitude $ map (/nd) $ iterateDiscreteFourierStep 1 cs
