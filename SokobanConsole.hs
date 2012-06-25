-- Copyright (c) 2012 Jonas Tullus
-- Licensed under MIT license (see LICENSE.txt)
-- Available on GitHub at http://github.com/jethr0/Sokoban
module Main where

import Sokoban
import Prelude hiding (Either(..))
import System.IO  (stdin, stdout, hSetEcho, hSetBuffering
                  ,BufferMode(..))
import Control.Monad (forM_, liftM)                  


instance Show World where
  show w = unlines chars
    where (maxX, maxY)  = wMax w
          chars         = [[func (x,y)  | x <- [0..maxX]] 
                                        | y <- [0..maxY]]
          func c 
            | isCrate   w c && isStorage w c  = '*'
            | isWorker  w c && isStorage w c  = '+'
            | isWall    w c                   = '#'
            | isWorker  w c                   = '@'
            | isCrate   w c                   = 'o'
            | isStorage w c                   = '.'
            | otherwise                       = ' '


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
  putStr "\ESC[2J"
  print world
  input <- getInput
  let world' = case modifyWorld world input of
                  Just x  -> x
                  Nothing -> world
  if isFinished world'
    then putStr "\ESC[2J" >> print world'
    else gameLoop world'
  

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  levels <- loadLevels "sokoban_levels.txt"
  forM_ (zip [1..] levels) $ \(n,l) -> do
    putStrLn $ "\n\nLevel " ++ show n ++ ":"
    gameLoop l


