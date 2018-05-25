module FastFourier (fastFourier) where

import Data.Complex
import Data.List (head, tail, zipWith, length)
import Data.List.Split (chunksOf)


toComplex :: Double -> (Complex Double)
toComplex a = (a :+ 0)


splitByOddAndEvenIdx :: [Double] -> ([Double], [Double])
splitByOddAndEvenIdx items =
    let
        chunks = chunksOf 2 items
        even = map head chunks
        odd = map last chunks
    in
        (even, odd)


getAngle :: Int -> Int -> Double
getAngle m k =
    if k == 0
        then 1
        else 2 * pi * (fromIntegral k) / (fromIntegral m)


getAngles :: Int -> [Int] -> [Double]
getAngles m ks = map (getAngle m) ks


fastFourier :: [Double] -> [Double]
fastFourier fs =
    case fs of
        [f] -> [f]
        fs ->
            let
                m = length fs
                k_max = (m `div` 2) - 1
                ks = [0..k_max]

                (even, odd) = splitByOddAndEvenIdx(fs)

                a_s = map toComplex $ fastFourier even
                b_s = fastFourier odd

                angles = getAngles m ks

                w_b_s = zipWith (mkPolar) b_s angles
                c_head = map magnitude $ zipWith (+) a_s w_b_s
                c_tail = map magnitude $ zipWith (-) a_s w_b_s
            in
                c_head ++ c_tail


