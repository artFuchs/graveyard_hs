{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

-- | This module contains the UI definition
module Editor.UIBuilders
( buildMainWindow
, buildMenubar
, buildTypeMenu
, buildHostMenu
, buildRuleMenu
, buildTreePanel
, buildHelpWindow
, showError
, createSaveDialog
, createCloseDialog
) where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified Data.Text as T
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import Data.Maybe
import Data.GI.Base
import Control.Monad.IO.Class

-- builds the main window, containing the treepanel in the left,
-- the canvas in the center,
-- the inspector panel in the right
-- and the menubar in the top of the window
buildMainWindow maybeMenuBar frameProps treePanel = do
  -- main window
  window <- new Gtk.Window [ #title         := "Graph Editor"
                          , #defaultWidth  := 640
                          , #defaultHeight := 480
                          ]

    -- creates a vBox to separate the editor and the menubar
  vBoxMain <- new Gtk.Box [ #orientation := Gtk.OrientationVertical
                          , #spacing := 0
                          ]

  Gtk.containerAdd window vBoxMain

  -- adds the menubar
  case maybeMenuBar of
    Just x -> Gtk.boxPackStart vBoxMain x False False 0
    Nothing -> return ()

  -- creates a HPane to add the treeView in the left
  hPaneTree <- new Gtk.Paned [ #orientation := Gtk.OrientationHorizontal ]
  Gtk.boxPackStart vBoxMain hPaneTree True True 0
  Gtk.panedPack1 hPaneTree treePanel False True

  -- creates a HPane to add the canvas in the left and the inspector panel in the right
  hPaneMain <- new Gtk.Paned [ #orientation := Gtk.OrientationHorizontal ]
  Gtk.panedPack2 hPaneTree hPaneMain True False
  Gtk.panedPack2 hPaneMain frameProps False True

  -- creates a frame to englobe the canvas
  frameCanvas <- new Gtk.Frame [ #shadowType := Gtk.ShadowTypeIn ]
  Gtk.panedPack1 hPaneMain frameCanvas True True
  -- creates a blank canvas
  canvas <- new Gtk.DrawingArea []
  Gtk.containerAdd frameCanvas canvas
  Gtk.widgetSetCanFocus canvas True
  Gtk.widgetSetEvents canvas [toEnum $ fromEnum Gdk.EventMaskAllEventsMask - fromEnum Gdk.EventMaskSmoothScrollMask]

  return (window, canvas, hPaneMain)



-- create the menu toolbar
buildMenubar = do
  return () :: IO ()
  builder <- new Gtk.Builder []
  Gtk.builderAddFromFile builder "./Resources/menubar.ui"
  menubar  <- Gtk.builderGetObject builder "menubar1" >>= unsafeCastTo Gtk.MenuBar . fromJust


  newItem <- Gtk.builderGetObject builder "new_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  openItem <- Gtk.builderGetObject builder "open_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  saveItem <- Gtk.builderGetObject builder "save_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  saveAsItem <- Gtk.builderGetObject builder "save_as_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  saveGraphItem <- Gtk.builderGetObject builder "save_graph_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  openGraphItem <- Gtk.builderGetObject builder "open_graph_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let fileItems = (newItem,openItem,saveItem,saveAsItem,saveGraphItem,openGraphItem)

  undoItem <- Gtk.builderGetObject builder  "undo_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  redoItem <- Gtk.builderGetObject builder  "redo_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  copyItem <- Gtk.builderGetObject builder  "copy_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  pasteItem <- Gtk.builderGetObject builder  "paste_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  cutItem <- Gtk.builderGetObject builder  "cut_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  sallItem <- Gtk.builderGetObject builder  "sall_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  snodesItem <- Gtk.builderGetObject builder  "snodes_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  sedgesItem <- Gtk.builderGetObject builder  "sedges_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let editItems = (undoItem,redoItem,copyItem,pasteItem,cutItem,sallItem,snodesItem,sedgesItem)

  zoomInItem <- Gtk.builderGetObject builder  "zoomin_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoomOutItem <- Gtk.builderGetObject builder  "zoomout_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom50Item <- Gtk.builderGetObject builder  "zoom50_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom100Item <- Gtk.builderGetObject builder  "zoom100_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom150Item <- Gtk.builderGetObject builder  "zoom150_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  zoom200Item <- Gtk.builderGetObject builder  "zoom200_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  resetViewItem <- Gtk.builderGetObject builder  "resetview_item" >>= unsafeCastTo Gtk.MenuItem . fromJust
  let viewItems = (zoomInItem,zoomOutItem,zoom50Item,zoom100Item,zoom150Item,zoom200Item,resetViewItem)

  helpItem <- Gtk.builderGetObject builder  "help_item" >>= unsafeCastTo Gtk.MenuItem . fromJust

  return (Just menubar, fileItems, editItems, viewItems, helpItem)

-- creates the inspector for typed graphs
buildTypeMenu :: IO (Gtk.Frame, Gtk.Entry, Gtk.ColorButton, Gtk.ColorButton, [Gtk.RadioButton], [Gtk.RadioButton], (Gtk.Box, Gtk.Frame, Gtk.Frame))
buildTypeMenu = do
  frame <- new Gtk.Frame [ #shadowType := Gtk.ShadowTypeIn ]
  mainBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 8
                         ]
  Gtk.containerAdd frame mainBox

  -- creates the title label
  inspectorLabel <- new Gtk.Label [ #label := "Inspector" ]
  Gtk.boxPackStart mainBox inspectorLabel False False 0

  -- creates a HBox containing a label and a entry for the user change the type name
  typeBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                         , #spacing := 8]
  Gtk.boxPackStart mainBox typeBox False False 0
  typeLabel <- new Gtk.Label [ #label := "Type: "]
  Gtk.boxPackStart typeBox typeLabel False False 0
  typeEntry <- new Gtk.Entry []
  Gtk.boxPackStart typeBox typeEntry True True 0
  Gtk.widgetSetCanFocus typeEntry True

  -- creates a HBox containing a label and ColorButton to the user change the node color
  colorBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                          , #spacing := 8]
  Gtk.boxPackStart mainBox colorBox False False 0
  colorLabel <- new Gtk.Label [ #label := "Fill color: "]
  Gtk.boxPackStart colorBox colorLabel False False 0
  colorButton <- new Gtk.ColorButton []
  Gtk.boxPackStart colorBox colorButton False False 0

  -- creates a HBox containing a label and a ColorButton to the user change the line and text color
  lineColorBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                              , #spacing := 8]
  Gtk.boxPackStart mainBox lineColorBox False False 0
  lineColorLabel <- new Gtk.Label [ #label := "Line color: "]
  Gtk.boxPackStart lineColorBox lineColorLabel False False 0
  lineColorButton <- new Gtk.ColorButton []
  Gtk.boxPackStart lineColorBox lineColorButton False False 0

  -- creates a frame containing a VBox with radio buttons to the user change the node shape
  frameShape <- new Gtk.Frame [#label := "Node Shape"]
  Gtk.boxPackStart mainBox frameShape False False 0
  nodeShapeBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical
                              , #spacing := 8]
  Gtk.containerAdd frameShape nodeShapeBox
  radioCircle <- new Gtk.RadioButton [#label := "Circle"]
  Gtk.boxPackStart nodeShapeBox radioCircle True True 0
  radioRect <- Gtk.radioButtonNewWithLabelFromWidget (Just radioCircle) "Rect"
  Gtk.boxPackStart nodeShapeBox radioRect True True 0
  radioQuad <- Gtk.radioButtonNewWithLabelFromWidget (Just radioCircle) "Quad"
  Gtk.boxPackStart nodeShapeBox radioQuad True True 0
  let radioShapes = [radioCircle, radioRect, radioQuad]


  -- creates a frame conataining a VBox with radioButtons to the user change the edge shape
  frameStyle <- new Gtk.Frame [#label := "Edge Style"]
  Gtk.boxPackStart mainBox frameStyle False False 0
  edgeStyleBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical
                              , #spacing := 8]
  Gtk.containerAdd frameStyle edgeStyleBox
  radioNormal <- new Gtk.RadioButton [#label := "Normal"]
  Gtk.boxPackStart edgeStyleBox radioNormal True True 0
  radioPointed <- Gtk.radioButtonNewWithLabelFromWidget (Just radioNormal) "Pointed"
  Gtk.boxPackStart edgeStyleBox radioPointed True True 0
  radioSlashed <- Gtk.radioButtonNewWithLabelFromWidget (Just radioNormal) "Slashed"
  Gtk.boxPackStart edgeStyleBox radioSlashed True True 0
  let radioStyles = [radioNormal, radioPointed, radioSlashed]

  return (frame, typeEntry, colorButton, lineColorButton, radioShapes, radioStyles, (colorBox, frameShape, frameStyle))

-- creates the inspector for the host graph
buildHostMenu :: IO ()
buildHostMenu = do
  return ()
  -- frame <- frameNew
  -- set frame [ frameShadowType := ShadowIn ]
  --
  -- vBoxProps <- vBoxNew False 8
  -- containerAdd frame vBoxProps
  --
  -- -- creates a title label
  -- titleLabel <- labelNew $ Just "Inspector"
  -- boxPackStart vBoxProps titleLabel PackNatural 0
  --
  -- -- creates a HBox containing a entry for the user change the node label
  -- hBoxLabel <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxLabel PackNatural 0
  -- labelLabel <- labelNew $ Just "Label: "
  -- entryLabel <- entryNew
  -- boxPackStart hBoxLabel labelLabel PackNatural 0
  -- boxPackStart hBoxLabel entryLabel PackGrow 0
  -- widgetSetCanFocus entryLabel True
  --
  -- -- creates a HBox containing a ComboBox for the user change the node type
  -- hBoxNodeType <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxNodeType PackNatural 0
  -- labelNodeType <- labelNew $ Just "Node Type: "
  -- boxPackStart hBoxNodeType labelNodeType PackNatural 0
  -- comboBoxNodeType <- comboBoxNewText
  -- boxPackStart hBoxNodeType comboBoxNodeType PackGrow 0
  --
  -- -- creates a HBox conataining a ComboBox for the user change the edge type
  -- hBoxEdgeType <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxEdgeType PackNatural 0
  -- labelEdgeType <- labelNew $ Just "Edge Type: "
  -- boxPackStart hBoxEdgeType labelEdgeType PackNatural 0
  -- comboBoxEdgeType <- comboBoxNewText
  -- boxPackStart hBoxEdgeType comboBoxEdgeType PackGrow 0
  --
  -- return (frame, entryLabel, comboBoxNodeType, comboBoxEdgeType, (hBoxNodeType, hBoxEdgeType))

buildRuleMenu :: IO ()
buildRuleMenu = do
  return ()
  --
  -- frame <- frameNew
  -- set frame [ frameShadowType := ShadowIn ]
  --
  -- vBoxProps <- vBoxNew False 8
  -- containerAdd frame vBoxProps
  --
  -- -- creates the title label
  -- titleLabel <- labelNew $ Just "Inspector"
  -- boxPackStart vBoxProps titleLabel PackNatural 0
  --
  -- -- creates a HBox containing a entry for the user change the node label
  -- hBoxLabel <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxLabel PackNatural 0
  -- labelLabel <- labelNew $ Just "Label: "
  -- entryLabel <- entryNew
  -- boxPackStart hBoxLabel labelLabel PackNatural 0
  -- boxPackStart hBoxLabel entryLabel PackGrow 0
  -- widgetSetCanFocus entryLabel True
  --
  -- -- creates a HBox containing a ComboBox for the user change the node type
  -- hBoxNodeType <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxNodeType PackNatural 0
  -- labelNodeType <- labelNew $ Just "Node Type: "
  -- boxPackStart hBoxNodeType labelNodeType PackNatural 0
  -- comboBoxNodeType <- comboBoxNewText
  -- boxPackStart hBoxNodeType comboBoxNodeType PackGrow 0
  --
  -- -- creates a HBox containing a ComboBox for the user change the edge type
  -- hBoxEdgeType <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxEdgeType PackNatural 0
  -- labelEdgeType <- labelNew $ Just "Edge Type: "
  -- boxPackStart hBoxEdgeType labelEdgeType PackNatural 0
  -- comboBoxEdgeType <- comboBoxNewText
  -- boxPackStart hBoxEdgeType comboBoxEdgeType PackGrow 0
  --
  -- -- creates a HBox containing a ComboBox for the user change the operation to be applyed in the graph element
  -- hBoxOperation <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxOperation PackNatural 0
  -- labelOperation <- labelNew $ Just "Operation: "
  -- boxPackStart hBoxOperation labelOperation PackNatural 0
  -- comboBoxOperation <- comboBoxNewText
  -- boxPackStart hBoxOperation comboBoxOperation PackGrow 0
  --
  -- return (frame, entryLabel, comboBoxNodeType, comboBoxEdgeType, comboBoxOperation, (hBoxNodeType, hBoxEdgeType))

-- creates the treePanel
buildTreePanel = do
  return () :: IO ()
  mainBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
  treeview <- new Gtk.TreeView [#headersVisible := True]
  Gtk.boxPackStart mainBox treeview True True 0

  col <- new Gtk.TreeViewColumn [#title := "project"]
  Gtk.treeViewAppendColumn treeview col

  renderer <- new Gtk.CellRendererText [#editable := True]
  Gtk.cellLayoutPackStart col renderer False

  btnNew <- new Gtk.Button [#label := "New Graph"]
  Gtk.boxPackStart mainBox btnNew False False 0

  btnRmv <- new Gtk.Button [#label := "Remove Graph"]
  Gtk.boxPackStart mainBox btnRmv False False 0

  return (mainBox, treeview, renderer, btnNew, btnRmv)



buildHelpWindow :: IO Gtk.Window
buildHelpWindow = do
  helpWindow <- new Gtk.Window [ #title := "Graph Editor - Help"]
  -- helpBuffer <- textBufferNew Nothing
  -- textBufferInsertAtCursor helpBuffer "<Creating and selecting>: \n"
  -- textBufferInsertAtCursor helpBuffer "Click with the right mouse button in a blank space to create a new node. \n"
  -- textBufferInsertAtCursor helpBuffer "Click with the left mouse button in a node/edge to select it. \n"
  -- textBufferInsertAtCursor helpBuffer "Click with the left button in a node/edge while the Shift key is pressed to add it to the selection. \n"
  -- textBufferInsertAtCursor helpBuffer "Click with the left button in a node/edge while the Shift and Ctrl keys are pressed to remove it from the selection. \n"
  -- textBufferInsertAtCursor helpBuffer "Click with the right button in a node while there's other nodes selected to create edges from the selected nodes to it. \n"
  -- textBufferInsertAtCursor helpBuffer "<Changing the node properties>: \n"
  -- textBufferInsertAtCursor helpBuffer "To change the properties of a node/edge, select it and use the inspector on the right. \n"
  -- textBufferInsertAtCursor helpBuffer "Double-clicking a node or edge, or pressing F2 will focus on the name entry box on the inspector panel. \n"
  -- textBufferInsertAtCursor helpBuffer "<zoom and navigation>: \n"
  -- textBufferInsertAtCursor helpBuffer "Use Ctrl + mouse wheel or Ctrl + [+/-] to change the zoom level. \n"
  -- textBufferInsertAtCursor helpBuffer "Use Ctrl + [=] to change the zoom level to the original. \n"
  -- textBufferInsertAtCursor helpBuffer "Hold the middle mouse button, or Ctrl + right mouse button, to navigate throught the canvas. \n"
  -- textBufferInsertAtCursor helpBuffer "Press ctrl + 0 to return to the initial position of the canvas and reset the zoom to the original. \n"
  -- helpView <- textViewNewWithBuffer helpBuffer
  -- containerAdd helpWindow helpView
  --
  return helpWindow


showError :: T.Text -> IO ()
showError msg = do
  --dlgE <- messageDialogNew window [DialogDestroyWithParent] MessageError ButtonsOk msg
  msgDialog <- new Gtk.MessageDialog [ #text := msg
                                , #messageType := Gtk.MessageTypeError
                                , #buttons := Gtk.ButtonsTypeOk
                                ]
  Gtk.widgetShowAll msgDialog
  Gtk.dialogRun msgDialog
  Gtk.widgetDestroy msgDialog
  return ()

createSaveDialog :: IO Gtk.FileChooserDialog
createSaveDialog = do
  saveD <- new Gtk.FileChooserDialog [ #action := Gtk.FileChooserActionSave
                                     , #createFolders := True
                                     , #doOverwriteConfirmation := True
                                     ]
  Gtk.dialogAddButton saveD "Save" (fromIntegral . fromEnum $ Gtk.ResponseTypeAccept)
  Gtk.dialogAddButton saveD "Cancel" (fromIntegral . fromEnum $ Gtk.ResponseTypeReject)
  return saveD

createCloseDialog :: T.Text -> IO Gtk.ResponseType
createCloseDialog msg = do
  closeD <- new Gtk.MessageDialog
            [ #text := msg
            , #messageType := Gtk.MessageTypeWarning
            , #buttons := Gtk.ButtonsTypeNone
            ]
  Gtk.dialogAddButton closeD "Save" (fromIntegral . fromEnum $ Gtk.ResponseTypeYes)
  Gtk.dialogAddButton closeD "Don't save" (fromIntegral . fromEnum $ Gtk.ResponseTypeNo)
  Gtk.dialogAddButton closeD "Cancel" (fromIntegral . fromEnum $ Gtk.ResponseTypeCancel)
  response <- Gtk.dialogRun closeD
  Gtk.widgetDestroy closeD
  return $ toEnum . fromIntegral $ response
