module Main where

import System.Environment (getArgs)
import Codec.Wav (exportFile, importFile)
import Data.Audio (Audio(Audio))
import Data.Array.Unboxed (elems)
import Data.Int (Int32)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import AudioUtils (getFirstChannel, saveAudio, readAudio)
import DiscreteFourier (discreteFourier, inverseDiscreteFourier)
import FastFourier


main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args


dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("mono", mono)
           , ("dft", dft)
           , ("fft", fft)
           ]


mono :: [String] -> IO ()
mono [inputFilePath, outputFilePath] = do
    (rate, audio) <- readAudio inputFilePath
    saveAudio outputFilePath rate audio


dft :: [String] -> IO ()
dft [inputFilePath, outputFilePath] = do

    putStrLn "Discrete fourier transform"
    (rate, audio) <- readAudio inputFilePath
    start <- getCurrentTime

    let result = inverseDiscreteFourier $ discreteFourier audio
    putStrLn $ "Input size " ++ (show $ length audio)
    putStrLn $ "Output size " ++ (show $ length result)
    saveAudio outputFilePath rate result

    end <- getCurrentTime
    putStrLn $ "Duration : " ++ (show $ (diffUTCTime end start))


fft :: [String] -> IO ()
fft [inputFilePath, outputFilePath] = do

    putStrLn "Fast fourier transform"
    (rate, audio) <- readAudio inputFilePath
    start <- getCurrentTime

    let result = fastFourier audio
    putStrLn $ "Input size " ++ (show $ length audio)
    putStrLn $ "Output size " ++ (show $ length result)
    saveAudio outputFilePath rate result

    end <- getCurrentTime
    putStrLn $ "Duration : " ++ (show $ (diffUTCTime end start))
