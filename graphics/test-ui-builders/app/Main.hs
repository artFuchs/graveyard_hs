{-
this application is just a test of the GtkBuilder.
it reads a xml file containing the definitions of a menubar and contruct the respective widget
-}

module Main where

import Graphics.UI.Gtk
import Control.Monad
import Control.Monad.IO.Class

main = do
  initGUI

  builder <- builderNew
  builderAddFromFile builder "./window.ui"

  -- create window
  window <- builderGetObject builder castToWindow "window"

  -- load the menubar
  menubar <- builderGetObject builder castToWidget "menubar1"

  newItem <- builderGetObject builder castToMenuItem "new_item"
  newItem `on` menuItemActivated $ putStrLn "new file"

  saveItem <- builderGetObject builder castToMenuItem "save_item"
  saveItem `on` menuItemActivated $ putStrLn "save file"

  openItem <- builderGetObject builder castToMenuItem "open_item"
  openItem `on` menuItemActivated $ putStrLn "open file"



  window `on` deleteEvent $ do
    liftIO $ mainQuit
    return False

  widgetShowAll window
  mainGUI
