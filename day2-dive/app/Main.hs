{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

data Direction = Forward Int | Down Int | Up Int

splitWs string = fromJust $ (`splitAt` string) <$> elemIndex ' ' string

parseDirection direction =
  let (dir, magn) = splitWs direction
   in ( case dir of
          "forward" -> Forward
          "down" -> Down
          "up" -> Up
      )
        $ read magn

addDirection item (horizontal, depth) = case item of
  Forward magn -> (horizontal + magn, depth)
  Up magn -> (horizontal, depth - magn)
  Down magn -> (horizontal, depth + magn)

finalPos :: [Direction] -> (Int, Int)
finalPos = foldr addDirection (0, 0)

getDirections fileName = do
  directions <- readFile fileName
  return $ map parseDirection $ lines directions

main = do
  directions <- getDirections "test_input.txt"
  let (horizontal, depth) = finalPos directions
   in print $ horizontal * depth