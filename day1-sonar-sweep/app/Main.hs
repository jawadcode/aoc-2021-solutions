module Main where

import GHC.Base (IO (IO))

numOfIncreasing (head : values) =
  let zipped = zip values (head : values)
   in sum $ map (\(a, b) -> if a > b then 1 else 0) zipped
numOfIncreasing [] = 0

getValues :: String -> IO [Integer]
getValues fileName = do
  fileContent <- readFile fileName
  return $ map read $ lines fileContent

main = do
  values <- getValues "test_input2.txt"
  print values
  print $ numOfIncreasing values