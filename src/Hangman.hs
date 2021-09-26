module Hangman where

cover :: String -> Char -> Char
cover guessed chr | chr `elem` guessed = chr
                  | otherwise          = '-'

guessing :: String -> String -> String
guessing guessed = map (cover guessed)


getWord :: IO String
getWord = do
    putStrLn "Welcome to Hangman!"
    putStr "Please, enter a word (ensure your friend can't see it): "
    getLine

allGuessed :: String -> String -> Bool
allGuessed guessed = all (`elem` guessed)

play :: String -> String -> Int -> IO ()
play word guessed attempts
  | allGuessed guessed word = do
      putStrLn $ "You won! Word was: " ++ word
  | attempts == 0 = do
      putStrLn $ "No more attempts. Word was: " ++ word
  | otherwise = do
      putStr $ "Guessed: " ++ guessing guessed word
      putStrLn $ " | Attempts: " ++ show attempts
      putStr "Enter your guess: "
      guess <- getLine
      if any (`elem` word) guess then
          play word (guessed ++ guess) attempts
      else
          play word guessed (attempts - 1)


main :: IO ()
main = do
    w <- getWord
    play w "" 6
