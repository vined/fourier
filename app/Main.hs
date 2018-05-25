module Main where

import System.Environment (getArgs)
import DiscreteFourier
import FastFourier


main :: IO ()
main = do
    (data_file_name:output_file_name:_) <- getArgs
    putStrLn $ "Reading audio file " ++ data_file_name

    putStrLn $ "Writing to audio file " ++ output_file_name
