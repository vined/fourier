module FastFourier (fastFourier, inverseFastFourier, getNewIdxOrder) where

import Data.Complex (Complex, cis, magnitude)
import Data.List (head, tail, foldl, zipWith, length, reverse)
import Data.List.Split (chunksOf)
import Data.Char (intToDigit, digitToInt)
import Numeric (showIntAtBase)

import ComplexUtils (toComplexM)


splitByOddAndEvenIdx :: [(Complex Double)] -> ([(Complex Double)], [(Complex Double)])
splitByOddAndEvenIdx items =
    let
        chunks = chunksOf 2 items
        even = map head chunks
        odd = map last chunks
    in
        (even, odd)


splitInHalf :: [(Complex Double)] -> ([(Complex Double)], [(Complex Double)])
splitInHalf items =
    let l = (length items) `div` 2
    in (take l items, drop l items)


getAngle :: Int -> Int -> Int -> (Complex Double)
getAngle op m k =
    if k == 0
        then cis 0
        else cis (2.0 * pi * (fromIntegral k) * (fromIntegral op) / (fromIntegral m))


getAngles :: Int -> Int -> [Int] -> [(Complex Double)]
getAngles op m ks = map (getAngle op m) ks


fastFourier' :: Int -> [(Complex Double)] -> [(Complex Double)]
fastFourier' op fs =
    case fs of
        [f] -> [f]
        fs ->
            let
                m = length fs
                k_max = (m `div` 2) - 1
                ks = [0..k_max]

--                 (even, odd) = splitByOddAndEvenIdx(fs)
                (even, odd) = splitInHalf(fs)

                a_s = fastFourier' op even
                b_s = fastFourier' op odd

                angles = getAngles op m ks

                w_b_s = zipWith (*) b_s angles
                c_head = zipWith (+) a_s w_b_s
                c_tail = zipWith (-) a_s w_b_s
            in
                c_head ++ c_tail


addZeros :: Num a => Int -> [a] -> [a]
addZeros maxSize arr =
        let diff = maxSize - (length arr)
        in arr ++ replicate diff 0


padWithZeros :: [Double] -> [Double]
padWithZeros fs =
    let
        len = length fs
        pow = ceiling $ logBase 2 $ fromIntegral len
        a = 2^pow
    in
        addZeros a fs


toInt :: String -> Int
toInt s = foldl (\acc x -> acc * 2 + digitToInt x) 0 s


toBinaryStr :: Int -> String
toBinaryStr i = showIntAtBase 2 intToDigit i ""


addStrZeros :: Int -> String -> String
addStrZeros maxSize arr =
        let diff = maxSize - (length arr)
        in arr ++ replicate diff '0'


binaryInverse :: Int -> Int -> Int
binaryInverse maxDigitCount i =
    toInt $ addStrZeros maxDigitCount $ reverse $ toBinaryStr i


getNewIdxOrder :: Int -> [Int]
getNewIdxOrder len =
    let
        n = len -1
        maxDigitCnt = length $ toBinaryStr n
    in
        map (binaryInverse maxDigitCnt) [0..n]


reorder :: [a] -> [a]
reorder fs =
    map (\x -> fs!!x) $ getNewIdxOrder $ length fs


fastFourier :: [Double] -> [(Complex Double)]
fastFourier fs = fastFourier' (-1) $ toComplexM $ reorder $ padWithZeros fs


prepareForInverse :: [(Complex Double)] -> [(Complex Double)]
prepareForInverse cs =
    let c0 = head cs
    in [c0] ++ (reverse $ tail cs)


inverseFastFourier :: [(Complex Double)] -> [Double]
inverseFastFourier cs =
    let
        n = length cs
        nd = fromIntegral n
    in
        map (/nd) $ map magnitude $ fastFourier' (1) $ reorder cs
