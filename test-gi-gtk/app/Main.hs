{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import Data.Text
import Data.IORef
import Control.Monad.IO.Class

import Data.GI.Base
import qualified GI.Gtk as Gtk

import qualified GI.Gdk as Gdk
import GI.Cairo
import Graphics.Rendering.Cairo
Graphics import.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Control.Monad.Trans.Reader (runReaderT)
import Foreign.Ptr (castPtr)


-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))


main :: IO ()
main = do
  -- init GTK lib
  Gtk.init Nothing
  
  zoom <- newIORef 1.0
  cairoctx <- newIORef Nothing
  
  -- init window
  window <- new Gtk.Window [ #title         := "Calculator"
                           , #resizable     := True
                           , #defaultWidth  := 230
                           , #defaultHeight := 250 ]

  on window #destroy Gtk.mainQuit
  
  -- add a box to the window
  mainbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical 
                         , #spacing := 8]
  #add window mainbox

  -- add a display saying hello
  display <- Gtk.entryNew
  set display [ #editable := True
              , #xalign   := 1
              , #text     := "Hello"]
  Gtk.boxPackStart mainbox display False False 0

  -- add a new dark area here
  canvas <- Gtk.drawingAreaNew
  Gtk.widgetSetEvents canvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]
  Gtk.boxPackStart mainbox canvas True True 0
  
  em <- Gtk.widgetGetEvents canvas
  print em
  
  
  on canvas #scrollEvent $ \event -> do 
    direction <- Gdk.getEventScrollDirection event
    liftIO $ case direction of 
      Gdk.ScrollDirectionUp -> do 
        modifyIORef zoom (\z -> z*1.1)
        Gtk.widgetQueueDraw canvas
      Gdk.ScrollDirectionDown -> do
        modifyIORef zoom (\z -> z*0.9)
        Gtk.widgetQueueDraw canvas
      _ -> putStrLn "other"
    return False
  
  Gtk.onWidgetDraw canvas $ \context -> do
    z <- liftIO$ readIORef zoom
    renderWithContext context $ myDraw z
    return False
    
    
    
    

  #showAll window

  Gtk.main

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
