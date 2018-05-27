module AudioUtils (getFirstChannel, saveAudio, readAudio) where

import Codec.Wav (importFile, exportFile)
import Data.Audio (Audio(Audio))
import Data.Array.Unboxed (bounds, listArray, elems)
import Data.Int (Int32)
import Data.List (head, length)
import Data.List.Split (chunksOf)
import System.IO (FilePath)
import System.Exit (exitFailure)


getFirstChannel :: Int -> [Int32] -> [Int32]
getFirstChannel channels audio = map head $ chunksOf channels audio


readAudio :: FilePath -> IO (Int, [Double])
readAudio path = do
    putStrLn $ "Reading audio file: " ++ path
    audio <- importFile path
    case audio :: Either String (Audio Int32) of
        Left msg -> do
            putStrLn $ "Failed to read audio: " ++ msg
            exitFailure
        Right (Audio rate channels samples) -> do
            putStrLn $ "Audio bit rate: " ++ show rate
            putStrLn $ "Audio channels: " ++ show channels

            return (rate, map fromIntegral $ getFirstChannel channels $ elems samples)


saveAudio :: FilePath -> Int -> [Double] -> IO ()
saveAudio path rate audio = do
    let l = (length audio) - 1
        maxInt32 :: Double
        maxInt32 = fromIntegral (maxBound::Int32)
        rounder :: Double -> Int32
        rounder = round . (*maxInt32)

    putStrLn $ "Writing to audio file: " ++ path
    exportFile path ( Audio rate 1 -- 1 channel
                  $ listArray (0,l)
                  $ map rounder
                  $ audio)
