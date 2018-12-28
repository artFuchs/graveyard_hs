-- drawing something in a canvas

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.Text

-- | Drawing State : escala deslocamento da origem
data DState = DState Float

-- GUI
main :: IO ()
main = do
  -- inicializa GTK
  initGUI


  -- inicializa janela principal
  window <- windowNew
  set window  [ windowTitle         := "Calculator"
              , windowResizable     := True
              , windowDefaultWidth  := 640
              , windowDefaultHeight := 480]


  -- inicializa um canvas na janela principal
  canvas <- drawingAreaNew
  widgetSetCanFocus canvas True
  widgetAddEvents canvas [AllEventsMask]
  containerAdd window canvas
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

  -- mostra widgets
  widgetShowAll window

  -- estado da aplicação
  zoom <- newIORef 1.0

  -- tratamento de eventos - canvas
  canvas `on` draw $ do
    z <- liftIO $ readIORef zoom
    myDraw z

  canvas `on` scrollEvent $ do
    dir <- eventScrollDirection
    liftIO $ do
      case dir of
        ScrollDown -> modifyIORef zoom (*0.9)
        ScrollUp  -> modifyIORef zoom (*1.1)
        _ -> return ()
      widgetQueueDraw canvas
    return True

  canvas `on` keyPressEvent $ do
    key <- eventKeyName
    liftIO $ do
      case unpack key of
        "minus" -> do
          modifyIORef zoom (*0.9)
          widgetQueueDraw canvas
        "plus"  -> do
          modifyIORef zoom (*1.1)
          widgetQueueDraw canvas
        "equal"  -> do
          writeIORef zoom 1.0
          widgetQueueDraw canvas
        _       -> return ()
    return True




 -- tratamento de eventos - janela
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  -- loop principal
  mainGUI

myDraw :: Double -> Render ()
myDraw z = do

    setSourceRGB 1 0 0
    setLineWidth 5

    scale z z

    moveTo 120 60
    lineTo 60 110
    lineTo 180 110
    closePath
    fill

    moveTo 60 110
    lineTo  0 160
    lineTo 120 160
    closePath
    fill

    moveTo 180 110
    lineTo 240 160
    lineTo 120 160
    closePath
    fill


    setSourceRGB 0 0 0
    stroke

    setLineWidth 1
    moveTo 400 400
    textPath (pack . show $ z)
    stroke
