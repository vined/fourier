module IOUtils where

import System.IO (FilePath, readFile, writeFile)
import Data.Complex (Complex)

parseLines :: [String] -> [Double]
parseLines ls = map read ls :: [Double]


readData :: FilePath -> IO [Double]
readData filePath = do
    content <- readFile filePath
    return $ parseLines $ lines content


writeData :: FilePath -> [Double] -> IO ()
writeData filePath d = writeFile filePath $ unlines $ map show d


printData :: Show a => [a] -> IO ()
printData d = putStrLn $ unlines $ map (++ ", ") $ map show d
