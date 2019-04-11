{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import Editor.UIBuilders
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text
import Control.Monad.IO.Class

setJustText :: Gtk.ListStore -> Gtk.TreeIter -> Text -> IO ()
setJustText store iter text = do
  gv <- toGValue (Just text)
  #set store iter [0] [gv]


main :: IO ()
main = do
  Gtk.init Nothing
  (treePanel, treeview, renderer, btnNew, btnRmv) <-buildTreePanel
  (inspectorFrame, typeLabel, cbtn, lcbtn, rshps, rstls, hideable) <- buildTypeMenu
  (menubar, fileItems, editItems, viewItems, helpItem) <- buildMenubar
  (window,canvas,_) <- buildMainWindow menubar inspectorFrame treePanel

  on window #deleteEvent $ return $ do
    response <- createCloseDialog "You want to save before quit?"
    continue <- case response of
      Gtk.ResponseTypeNo -> return True
      Gtk.ResponseTypeYes -> do
        showError "can't save!"
        return True
      _ -> return False
    case continue of
      True -> do
        Gtk.mainQuit
        return False
      False -> return True

  store <- Gtk.listStoreNew [gtypeString]
  iter <- Gtk.listStoreAppend store
  setJustText store iter "new"


  projectCol <- Gtk.treeViewGetColumn treeview 0
  case projectCol of
    Nothing -> return ()
    Just col -> do
      Gtk.treeViewSetModel treeview (Just store)
      --Gtk.cellLayoutSetAttributes col renderer store $ \name -> [#text := name]
      #addAttribute col renderer "text" 0
      --treepath <- new Gtk.TreePath [0]
      --Gtk.treeViewSetCursor treeview treepath Nothing False

  Gtk.widgetShowAll window

  on btnNew #clicked $ do
    showError "Can't create new graph. Saving instead..."
    saveD <- createSaveDialog
    response <- Gtk.dialogRun saveD
    case toEnum . fromIntegral $ response of
      Gtk.ResponseTypeAccept -> showError "Save not implemented yet!!"
      _ -> return ()
    #destroy saveD
    return ()
  on btnRmv #clicked $ showError "This too was Not implemented yet!"

  Gtk.main
  return ()
