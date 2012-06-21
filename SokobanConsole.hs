module Main where

import Sokoban
import Prelude hiding (Either(..))
import System.IO  (stdin, stdout, hSetEcho, hSetBuffering
                  ,BufferMode(..))


-- get characters until valid character is received
getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'w' -> return Up
    'a' -> return Left
    's' -> return Down
    'd' -> return Right
    otherwise -> getInput


gameLoop world = do
  print world
  input <- getInput
  let world' = case modifyWorld world input of
                  Just x  -> x
                  Nothing -> world
  if isFinished world'
    then print world' >> print "well done!"
    else gameLoop world'
  

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  gameLoop $ loadLevel level


