-- drawing something in a canvas

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

-- GUI
main :: IO ()
main = do
  -- inicializa GTK
  initGUI

  -- inicializa janela principal
  window <- windowNew
  set window  [ windowTitle         := "Calculator"
              , windowResizable     := True
              , windowDefaultWidth  := 230
              , windowDefaultHeight := 250]
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  -- adiciona canvas em branco
  vbox <- vBoxNew False 10
  canvas <- drawingAreaNew
  lCanvas <- labelNew $ Just "-- Canvas --"
  lCanvasEnd <- labelNew $ Just "-- End -- "
  boxPackStart vbox canvas PackNatural 0
  boxPackStart vbox lCanvas PackNatural 0
  boxPackEnd vbox lCanvasEnd PackNatural 0
  containerAdd window vbox


  -- show things
  widgetShowAll window
  mainGUI
