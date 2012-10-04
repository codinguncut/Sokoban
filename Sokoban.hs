-- Copyright (c) 2012 Johannes Ahlmann
-- Licensed under MIT license (see LICENSE.txt)
-- Available on GitHub at http://github.com/jethr0/Sokoban
module Sokoban  (modifyWorld
                ,isFinished
                ,loadLevels
                ,World(wMax, wSteps)
                ,Input(..)
                ,isWall
                ,isCrate
                ,isStorage
                ,isWorker
                ) where

import Prelude hiding (Either(..))
import Data.List (sort, delete, unfoldr)
import Control.Monad (forM_, liftM)

data Input  = Up 
            | Down 
            | Left 
            | Right
            deriving (Show, Eq, Ord)

type Coord = (Int, Int)

data World = World  {wWalls
                    ,wCrates
                    ,wStorages  :: [Coord]
                    ,wWorker    :: Coord
                    ,wMax       :: Coord
                    ,wSteps     :: Int
                    }

emptyWorld = World {wWalls    = []
                   ,wCrates   = []
                   ,wStorages = []
                   ,wWorker   = (0,0)
                   ,wMax      = (0,0)
                   ,wSteps    = 0
                   }

add :: Coord -> Input -> Coord
add (x,y) input = 
  case input of
    Up    -> (x  , y-1)
    Down  -> (x  , y+1)
    Left  -> (x-1, y  )
    Right -> (x+1, y  )

isWall :: World -> Coord -> Bool
isWall world coord = elem coord (wWalls world)

isCrate :: World -> Coord -> Bool
isCrate world coord = elem coord (wCrates world)

isStorage :: World -> Coord -> Bool
isStorage world coord = elem coord (wStorages world)

isWorker :: World -> Coord -> Bool
isWorker w c  = wWorker w == c


---

loadLevels :: String -> IO [World]
loadLevels filename = do
  lns <- liftM lines . readFile $ filename
  return $ unfoldr consume lns
  where isEmptyLine = all (' '==)
        consume [] = Nothing
        consume ls = let (a,b) = break isEmptyLine ls
                     in return (loadLevel $ unlines a, drop 1 b)


loadLevel :: String -> World
loadLevel str = foldl consume (emptyWorld{wMax = maxi}) elems
  where lns     = lines str
        coords  = [[(x,y) | x <- [0..]] | y <- [0..]]
        elems   = concat $ zipWith zip coords lns
        maxX    = maximum . map (fst . fst) $ elems
        maxY    = maximum . map (snd . fst) $ elems
        maxi    = (maxX, maxY)
        consume wld (c, elt) = 
          case elt of
            '@' -> wld{wWorker    = c}
            'o' -> wld{wCrates    = c:wCrates wld}
            '#' -> wld{wWalls     = c:wWalls wld}
            '.' -> wld{wStorages  = c:wStorages wld} 
            '*' -> wld{wCrates    = c:wCrates wld
                      ,wStorages  = c:wStorages wld}
            '+' -> wld{wStorages  = c:wStorages wld
                      ,wWorker    = c}
            ' ' -> wld
            otherwise -> error (show elt ++ " not recognized")


modifyWorld :: World -> Input -> Maybe World
modifyWorld world input 
  | isWall    world newPos  = Nothing
  | isCrate   world newPos  = 
      if isCrate world newPos' || isWall world newPos'
        then Nothing
        else return $ moveCrate world' newPos newPos'
  | otherwise               = return world'
  where moveCrate w old new = w{wCrates = new:delete old (wCrates w)}
        
        oldPos  = wWorker world
        newPos  = add oldPos input
        newPos' = add newPos input
        world'  = world{wWorker = newPos, wSteps = wSteps world + 1}
        

isFinished :: World -> Bool
isFinished world = 
  sort (wCrates world) == sort (wStorages world)
