module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

type WordList = [String]

minWordLength = 4
maxWordLength = 8

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

-- justRight :: Int -> Int -> String -> Bool
-- justRight min max word
--   | length word > min && length word < max = True
--   | otherwise = False

justRight :: String -> Bool
justRight word
  | length word >= minWordLength && length word <= maxWordLength = True
  | otherwise = False

gameWords :: IO WordList
gameWords = do
   aw <- allWords
   return (filter justRight aw)

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO(0, length wl)
  return $ wl !! randomIndex

main :: IO ()
main = do
  words <- gameWords
  word <- randomWord words
  putStrLn $ show word
