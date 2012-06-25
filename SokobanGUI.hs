-- Copyright (c) 2012 Jonas Tullus
-- Licensed under MIT license (see LICENSE.txt)
-- Available on GitHub at http://github.com/jethr0/Sokoban
module Main where

import Sokoban
import Prelude hiding (Either(..))
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import qualified Control.Concurrent.MVar as MV
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Control.Monad (when, liftM, forM_, forM)
import Control.Monad.IO.Class (liftIO)


data State = State  {sWorld   :: World
                    ,sLevels  :: [World]
                    ,sCurrent :: Int
                    }


emptyState = do
  levels <- loadLevels "sokoban_levels.txt"
  return $ State  {sWorld = levels!!0
                  ,sLevels = levels
                  ,sCurrent = 0
                  }


nextLevel state = do
  s <- MV.readMVar state
  if length (sLevels s) <= (sCurrent s) + 1
    then print "you have finished Sokoban"
    else MV.modifyMVar_ state $ \_ -> 
          return $ s{sWorld   = (sLevels s)!!((sCurrent s)+1)
                    ,sCurrent = (sCurrent s) + 1}


handleKeyboard window state = do
  tryEvent $ do
    "Left" <- (liftIO . keyvalName) =<< eventKeyVal
    checkInput Left
          
  tryEvent $ do
    "Right" <- (liftIO . keyvalName) =<< eventKeyVal
    checkInput Right
          
  tryEvent $ do
    "Up" <- (liftIO . keyvalName) =<< eventKeyVal
    checkInput Up
          
  tryEvent $ do
    "Down" <- (liftIO . keyvalName) =<< eventKeyVal
    checkInput Down
 
  -- reset level
  tryEvent $ do
    "r" <- (liftIO . keyvalName) =<< eventKeyVal
    liftIO $ do
      MV.modifyMVar_ state $ \s ->
        return s{sWorld = (sLevels s)!!(sCurrent s)}
      widgetQueueDraw window
  
  -- skip to next level
  tryEvent $ do
    "n" <- (liftIO . keyvalName) =<< eventKeyVal
    liftIO $ do
      nextLevel state
      widgetQueueDraw window
  
  tryEvent $ do
    "q" <- (liftIO . keyvalName) =<< eventKeyVal
    liftIO mainQuit
          
  where checkInput i = liftIO $ do
          updateWorld state i
          widgetQueueDraw window


updateWorld :: MV.MVar State -> Input -> IO ()
updateWorld state input = do
  s <- MV.readMVar state
  let world   = sWorld s
      world'  = case modifyWorld world input of
                  Nothing -> world
                  Just x  -> x
  MV.modifyMVar_ state (\s -> return s{sWorld = world'})
  when (isFinished world') $ nextLevel state


drawWindow window state tiles = liftIO $ do
  cr <- widgetGetDrawWindow window
  world <- liftM sWorld $ MV.readMVar state
  let (multx, multy) = (100, 85) -- tiles are 100x85
  renderWithDrawable cr $ do
    C.scale 0.4 0.4

    let (maxX, maxY) = wMax world
    let coords = [(x,y)  | x <- [0..maxX], y <- [0..maxY]]
    let lookup_ = Maybe.fromJust . (`M.lookup` tiles)
    let showAt what (x, y) = do
          C.setSourceSurface  (lookup_ what) 
                              (fromIntegral x * multx)
                              (fromIntegral y * multy) 
          C.paint
    let storage c = showAt "Selector"       c
    let worker  c = showAt "Character Boy"  c
    let crate   c = showAt "Gem Blue"       c
    let wall    c = showAt "Stone Block"    c
    let grass   c = return () --showAt "Grass Block"    c

    forM coords $ \c ->
      case () of () | isStorage world c && isCrate world c -> 
                        grass c >> storage c >> crate c
                    | isStorage world c && isWorker world c ->
                        grass c >> storage c >> worker c
                    | isStorage world c -> grass c >> storage c
                    | isCrate   world c -> grass c >> crate   c
                    | isWorker  world c -> grass c >> worker  c
                    | isWall    world c -> wall  c
                    | otherwise         -> grass c
  return True


loadTiles strings = do
  let longStrings = map (\s -> "images/" ++ s ++ ".png") strings
  surfaces <- mapM C.imageSurfaceCreateFromPNG longStrings
  return $ M.fromList $ zip strings surfaces


main :: IO ()
main = do
  initGUI

  state <- MV.newMVar =<< emptyState
  tiles <- loadTiles  ["Character Boy"
                      ,"Gem Blue"
                      ,"Grass Block"
                      ,"Plain Block"
                      ,"Selector"
                      ,"Stone Block"
                      ,"Stone Block Tall"
                      ]

  window <- windowNew
  window `on` sizeRequest   $ return (Requisition 800 600)
  window `on` keyPressEvent $ handleKeyboard window state
  window `on` exposeEvent   $ drawWindow window state tiles

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
