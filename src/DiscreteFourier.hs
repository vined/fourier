module DiscreteFourier (discreteFourier, inverseDiscreteFourier) where

import Data.Complex
import Data.List (zipWith, length)


getAngles :: Double -> Int -> [Double]
getAngles pi2k n = map (pi2k *) $ map fromIntegral [0..n-1]


discreteFourierStep :: [Double] -> Int -> Int -> (Complex Double)
discreteFourierStep fs op k =
    let
        n = length fs
    in
        if k == 0
            then (((sum fs) / fromIntegral n) :+ 0)
            else
                let
                    pi2k = 2 * pi * (fromIntegral k) * (fromIntegral op)
                    angles = getAngles pi2k n
                in
                    sum $ zipWith (mkPolar) fs angles


iterateDiscreteFourierStep :: [Double] -> Int -> [(Complex Double)]
iterateDiscreteFourierStep fs op =
    let
        n = length fs
    in
        map (discreteFourierStep fs op) [0..n-1]


discreteFourier :: [Double] -> [Double]
discreteFourier fs =
    let
        n = length fs
        nd = fromIntegral n
    in
        map magnitude $ map (/nd) $ iterateDiscreteFourierStep fs (-1)


inverseDiscreteFourier :: [Double] -> [Double]
inverseDiscreteFourier fs =
    let
        n = length fs
    in
        map magnitude $ iterateDiscreteFourierStep fs 1



