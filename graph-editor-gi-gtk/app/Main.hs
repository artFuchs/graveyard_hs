{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import Editor.UIBuilders
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text
import Control.Monad.IO.Class
import Graphics.Rendering.Cairo
import Editor.Render
import Editor.GraphicalInfo

setJustText :: Gtk.ListStore -> Gtk.TreeIter -> Text -> IO ()
setJustText store iter text = do
  gv <- toGValue (Just text)
  #set store iter [0] [gv]


main :: IO ()
main = do
  Gtk.init Nothing
  (treePanel, treeview, renderer, btnNew, btnRmv) <-buildTreePanel
  --(inspectorFrame, typeLabel, cbtn, lcbtn, rshps, rstls, hideable) <- buildTypeMenu
  --(inspectorFrame, entryLabel, comboBoxNodeType, comboBoxEdgeType, (hBoxNodeType, hBoxEdgeType)) <- buildHostMenu
  (inspectorFrame, labelEntry, nodeTypeComboBox, edgeTypeComboBox, operationComboBox, (nodeTypeBox, edgeTypeBox)) <- buildRuleMenu
  (menubar, fileItems, editItems, viewItems, helpItem) <- buildMenubar
  (window,canvas,_) <- buildMainWindow menubar inspectorFrame treePanel

  on window #destroy Gtk.mainQuit

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

  on canvas #draw $ \context -> do
    renderWithContext context drawGraph
    return False

  Gtk.main
  return ()

drawGraph :: Render ()
drawGraph = do
  let ngi1 = NodeGI {position = (100,100), fillColor = (1,1,1), lineColor = (0,0,0), dims = (20,20), shape = NSquare}
      ngi2 = NodeGI {position = (200,200), fillColor = (1,1,1), lineColor = (0,0,0), dims = (20,20), shape = NCircle}
      egi = EdgeGI {cPosition = (150,150), color = (0,0,0), centered = True, style = ENormal}
  renderNode ngi1 "a" False
  renderNode ngi2 "b" False
  renderEdge egi "ab" False ngi1 ngi2
