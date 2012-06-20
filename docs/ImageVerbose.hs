-- ImageVerbose from Haskell Wolfenstein
-- Licensed under GPL v3
-- source: http://code.google.com/p/haskellstolfenwein/source/browse/trunk/beispiele/09Images/ImageVerbose.hs?spec=svn8&r=8
-- Modified by Jonas Tullus, 2012-06-20

module Main (main) where

{-
import Graphics.Rendering.Cairo(Surface, setSourceSurface, paint, imageSurfaceCreateFromPNG)
import Graphics.UI.Gtk(Window,AttrOp((:=)),Requisition(..)
                      ,WindowPosition(WinPosCenter)
                      ,initGUI,windowNew,onExpose,onDestroy,mainQuit,set
                      ,windowWindowPosition,onSizeRequest,widgetShowAll
                      ,mainGUI,widgetGetDrawWindow,renderWithDrawable)
import Graphics.UI.Gtk.Gdk.Events(Event)
-}
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

onExposeEvent:: Window-> Surface-> Event-> IO Bool
onExposeEvent window image _ = do
  cr <-widgetGetDrawWindow window
  renderWithDrawable cr $ do
    save
    translate 100 100
    rectangle 0 0 100 100 -- left right width height
    clip
    newPath
    setSourceSurface image 0 0
    paint
    restore

    save
    translate 200 200
    rectangle 100 100 100 100 -- left right width height
    clip
    newPath
    setSourceSurface image 0 0
    paint
    restore
  return True


main:: IO ()
main = do
  image <-imageSurfaceCreateFromPNG "test.png"
  initGUI
  window <-windowNew
  onExpose window (onExposeEvent window image)
  onDestroy window mainQuit
  set window [ windowWindowPosition := WinPosCenter ]
  window `onSizeRequest` return (Requisition 320 250)
  widgetShowAll window
  mainGUI

