-- ImageVerbose from Haskell Wolfenstein
-- Licensed under GPL v3
-- source: http://code.google.com/p/haskellstolfenwein/source/browse/trunk/beispiele/09Images/ImageVerbose.hs?spec=svn8&r=8
-- Modified by Jonas Tullus, 2012-06-20

module Main (main) where

import Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Control.Concurrent.MVar as MV

onExposeEvent:: Window -> Surface -> MV.MVar Double -> EventM EExpose Bool
onExposeEvent window image state = liftIO $ do
  s <- MV.readMVar state
  cr <- widgetGetDrawWindow window
  (w, h) <- widgetGetSize window

  -- double buffering
  -- regio <- regionRectangle $ Rectangle 0 0 w h
  -- drawWindowBeginPaintRegion cr regio

  renderWithDrawable cr $ do
    C.save
    C.translate s s
    C.rectangle 0 0 100 100 -- left right width height
    C.clip
    C.newPath
    C.setSourceSurface image 0 0
    C.paint
    C.restore

  -- double buffering
  -- drawWindowEndPaint cr

  return True


main:: IO ()
main = do
  state <- MV.newMVar 100

  image <- imageSurfaceCreateFromPNG "test.png"
  initGUI
  window <- windowNew

  --canvas <- drawingAreaNew
  --containerAdd window canvas

  window `on` sizeRequest $ return (Requisition 800 600)
  window `on` exposeEvent $ onExposeEvent window image state
  
  window `on` keyPressEvent $ do
    tryEvent $ do
      "Return" <- eventKeyName
      liftIO $ do
        MV.modifyMVar_ state (return . (10+))
        widgetQueueDraw window
    tryEvent $ do
      val <- liftIO $ keyvalFromName "Left"
      val <- eventKeyVal
      liftIO $ do
        MV.modifyMVar_ state (return . (`subtract` 100))
        widgetQueueDraw window

  (`timeoutAdd` (1000 `div` 60)) $ liftIO $ do
    MV.modifyMVar_ state (return . (1+))
    widgetQueueDraw window
    return True

  onDestroy window mainQuit
  set window [ windowWindowPosition := WinPosCenter ]
  widgetShowAll window
  mainGUI

