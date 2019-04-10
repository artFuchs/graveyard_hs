{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import Editor.UIBuilders
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text

setJustText :: Gtk.ListStore -> Gtk.TreeIter -> Text -> IO ()
setJustText store iter text = do
  gv <- toGValue (Just text)
  #set store iter [0] [gv]


main :: IO ()
main = do
  Gtk.init Nothing
  (treePanel, treeview, renderer, btnNew, btnRmv) <-buildTreePanel
  (inspectorFrame, typeLabel, cbtn, lcbtn, rshps, rstls, hideable) <- buildTypeMenu
  (window,canvas,_) <- buildMainWindow (Nothing :: Maybe Gtk.Widget) inspectorFrame treePanel

  on window #destroy $ Gtk.mainQuit

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
