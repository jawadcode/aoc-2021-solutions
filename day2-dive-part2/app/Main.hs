{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Direction
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

splitWs :: String -> (String, String)
splitWs string = fromJust $ (`splitAt` string) <$> elemIndex ' ' string

parseDirection :: String -> Direction
parseDirection direction =
  let (dir, n) = splitWs direction
   in ( case dir of
          "forward" -> Forward
          "down" -> Down
          "up" -> Up
      )
        $ read n

addDirection :: (Int, Int, Int) -> Direction -> (Int, Int, Int)
addDirection (aim, horizontal, depth) item = case item of
  Forward n -> (aim, horizontal + n, depth + aim * n)
  Down n -> (aim + n, horizontal, depth)
  Up n -> (aim - n, horizontal, depth)

finalPos :: [Direction] -> (Int, Int, Int)
finalPos = foldl addDirection (0, 0, 0)

getDirections :: String -> IO [Direction]
getDirections fileName = do
  directions <- readFile fileName
  return $ map parseDirection $ lines directions

main :: IO ()
main = do
  directions <- getDirections "test_input.txt"
  let (_, horizontal, depth) = finalPos directions
   in print $ horizontal * depth
