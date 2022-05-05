module HaskellBook.Chapter14 where

import qualified Data.Map as M
import           Control.Monad (forever, when)
import           Data.List (intercalate)
import           Data.Traversable (traverse)
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (hGetLine, hIsEOF, stdin, isEOF)

type Morse = String

myAdd :: (Num a) => a -> a -> a
myAdd x y = x + y

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList
  [ ('a', ".-")
  , ('b', "-...")
  , ('c', "-.-.")
  , ('d', "-..")
  , ('e', ".")
  , ('f', "..-.")
  , ('g', "--.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".---")
  , ('k', "-.-")
  , ('l', ".-..")
  , ('m', "--")
  , ('n', "-.")
  , ('o', "---")
  , ('p', ".--.")
  , ('q', "--.-")
  , ('r', ".-.")
  , ('s', "...")
  , ('t', "-")
  , ('u', "..-")
  , ('v', "...-")
  , ('w', ".--")
  , ('x', "-..-")
  , ('y', "-.--")
  , ('z', "--..")
  , ('1', ".----")
  , ('2', "..---")
  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-....")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , ('0', "-----")]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = mapM charToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

convertToMorse :: IO ()
convertToMorse = forever
  $ do
    weAreDone <- isEOF
    when weAreDone exitSuccess
    line <- getLine
    convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just str) -> putStrLn (unwords str)
        Nothing    -> do
          putStrLn $ "Error: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever
  $ do
    weAreDone <- isEOF
    when weAreDone exitSuccess
    line <- getLine
    convertLine line
  where
    convertLine line = do
      let decoded :: Maybe String
          decoded = traverse morseToChar (words line)
      case decoded of
        Nothing -> do
          putStrLn $ "Error: " ++ line
          exitFailure
        Just s  -> putStrLn s

morseMain :: IO ()
morseMain = do
  mode <- getArgs
  case mode of
    [arg] -> case arg of
      "from" -> convertFromMorse
      "to"   -> convertToMorse
      _      -> argError
    _     -> argError
  where
    argError = do
      putStrLn
        "Please specify the\
            \ first argument\
            \ as being 'from' or\
            \ 'to' morse,\
            \ such as: morse to"
      exitFailure
