module Main where

import Data.Bits (Bits (complement, shiftL, xor))
import Data.Char (ord)
import Data.Foldable (Foldable (foldl'))
import GHC.Float (int2Float)

countDigits :: [Int] -> String -> [Int]
countDigits oneCounts number =
  let zipped = zip oneCounts number
   in map (\(num, digit) -> num + ord digit - 48) zipped

getDigits :: [String] -> [Int]
getDigits (head : tail) =
  foldl' countDigits [ord c - 48 | c <- head] tail
getDigits [] = []

toBin :: Int -> [Int] -> [Int]
toBin len =
  map (\digit -> if int2Float digit > int2Float len / 2 then 1 else 0)

getGamma :: Int -> [Int] -> Int
getGamma len numbers =
  foldl' (\acc digit -> 2 * acc + digit) 0 $ toBin len numbers

getNumbers :: String -> IO [String]
getNumbers fileName = do
  content <- readFile fileName
  return $ lines content

main :: IO ()
main = do
  numbers <- getNumbers "test_input.txt"
  let len = length numbers
      digits = getDigits numbers
      gamma = getGamma len digits
      epsilon = gamma `xor` sum [1 `shiftL` x | x <- [0 .. (length (head numbers) - 1)]]
   in print $ gamma * epsilon