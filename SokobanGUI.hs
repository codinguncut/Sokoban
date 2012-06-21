module Main where

import Sokoban
import Prelude hiding (Either(..))
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import qualified Control.Concurrent.MVar as MV
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Control.Monad (when, liftM, forM_)
import Control.Monad.IO.Class (liftIO)


data State = State  {sWorld :: World
                    }

emptyState = State  {sWorld = loadLevel level
                    }


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
          
  where checkInput i = liftIO $ do
          updateWorld state i
          widgetQueueDraw window


updateWorld state input = do
  s <- MV.readMVar state
  let world   = sWorld s
      world'  = case modifyWorld world input of
                  Nothing -> world
                  Just x  -> x
  when (isFinished world') $ print "finished"
  MV.modifyMVar_ state (\s -> return s{sWorld = world'})


drawWindow window state tiles = liftIO $ do
  cr <- widgetGetDrawWindow window
  world <- liftM sWorld $ MV.readMVar state
  -- tiles are 100x85
  let multx = 100
  let multy = 85
  renderWithDrawable cr $ do
    C.scale 0.4 0.4

    let showAt what (x, y) = do
          C.setSourceSurface what (fromIntegral x * multx)
                                  (fromIntegral y * multy) 
          C.paint

    let storage = Maybe.fromJust $ M.lookup "Selector" tiles
    forM_ (reverse $ wStorages world) (showAt storage)

    let worker = Maybe.fromJust $ M.lookup "Character Boy" tiles
    showAt worker (wWorker world)

    let crate = Maybe.fromJust $ M.lookup "Gem Blue" tiles
    forM_ (reverse $ wCrates world) (showAt crate)
    
    let wall = Maybe.fromJust $ M.lookup "Stone Block" tiles
    forM_ (reverse $ wWalls world) (showAt wall)
    
  return True


loadTiles strings = do
  let longStrings = map (\s -> "images/" ++ s ++ ".png") strings
  surfaces <- mapM C.imageSurfaceCreateFromPNG longStrings
  return $ M.fromList $ zip strings surfaces


main :: IO ()
main = do
  initGUI

  state <- MV.newMVar emptyState
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
