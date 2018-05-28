module Main where

import System.Environment (getArgs)
import Codec.Wav (exportFile, importFile)
import Data.Audio (Audio(Audio))
import Data.Array.Unboxed (elems)
import Data.Int (Int32)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import AudioUtils (getFirstChannel, saveAudio, readAudio)
import IOUtils (readData, writeData, printData)
import DiscreteFourier (discreteFourier, inverseDiscreteFourier)
import FastFourier (fastFourier, inverseFastFourier, getNewIdxOrder)


main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args


dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("mono", mono)
           , ("dft-vs-fft", dftVsFft)
           , ("dft-audio", dftAudio)
           , ("fft-audio", fftAudio)
           ]


mono :: [String] -> IO ()
mono [inputFilePath, outputFilePath] = do
    (rate, audio) <- readAudio inputFilePath
    saveAudio outputFilePath rate audio


dftVsFft:: [String] -> IO ()
dftVsFft [inputFilePath, outputFilePath] = do

    putStrLn "Discrete fourier transform"

    d <- readData inputFilePath
    putStrLn $ "Input size " ++ (show $ length d)

    start <- getCurrentTime
    let rdft = discreteFourier d
--     printData rdft

    end <- getCurrentTime
    putStrLn $ "Duration : " ++ (show $ (diffUTCTime end start))

    let ridft = inverseDiscreteFourier rdft
--     printData ridft

    start <- getCurrentTime
    let rfft = fastFourier d
--     printData rfft

    end <- getCurrentTime
    putStrLn $ "Duration : " ++ (show $ (diffUTCTime end start))

    let rifft = inverseFastFourier rfft
--     printData rifft
    putStrLn "Done."


dftAudio :: [String] -> IO ()
dftAudio [inputFilePath, outputFilePath] = do

    putStrLn "Discrete fourier audio transform"
    (rate, audio) <- readAudio inputFilePath
    putStrLn $ "Input size " ++ (show $ length audio)

    start <- getCurrentTime
    let result = inverseDiscreteFourier $ discreteFourier audio
    saveAudio outputFilePath rate result

    end <- getCurrentTime
    putStrLn $ "Duration : " ++ (show $ (diffUTCTime end start))


fftAudio :: [String] -> IO ()
fftAudio [inputFilePath, outputFilePath, filterFromHz, filterToHz] = do

    putStrLn "Fast fourier audio transform"
    (rate, audio) <- readAudio inputFilePath
    putStrLn $ "Input size " ++ (show $ length audio)

    start <- getCurrentTime
    let result = inverseFastFourier $ fastFourier audio
    saveAudio outputFilePath rate result

    end <- getCurrentTime
    putStrLn $ "Duration : " ++ (show $ (diffUTCTime end start))
