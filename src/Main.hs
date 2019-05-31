module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

minWordLength = 4
maxWordLength = 8

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (fmap (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) guess =
  Puzzle word newFilledInSoFar (guess:s) where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
newFilledInSoFar = zipWith (zipper guess) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that"
      return puzzle
    (True, _) -> do
      putStrLn "In the word!"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "Nope"
      return (fillInCharacter puzzle guess)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

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
  randomIndex <- randomRIO(0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if ( length guessed) > 7 then
    do putStrLn "You lost"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

main :: IO ()
main = do
  words <- gameWords
  word <- randomWord words
  putStrLn $ show word
