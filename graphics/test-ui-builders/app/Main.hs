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

  -- create window
  window <- windowNew
  set window [ windowTitle        := "hohohoho"
             , windowDefaultWidth := 300
             , windowDefaultHeight := 200
             ]

  vboxMain <- vBoxNew False 8
  containerAdd window vboxMain

  -- load the menubar
  builder <- builderNew
  builderAddFromFile builder "./menubar.ui"
  menubar <- builderGetObject builder castToWidget "menubar1"
  boxPackStart vboxMain menubar PackNatural 0

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
