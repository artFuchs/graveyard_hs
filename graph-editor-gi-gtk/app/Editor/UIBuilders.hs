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
import Data.GI.Base

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
buildMenubar :: IO ()
buildMenubar = return ()
  -- builder <- new Gtk.Builder []
  -- Gtk.builderAddFromFile builder "./Resources/menubar.ui"
  -- menubar <- builderGetObject builder castToWidget "menubar1"
  --
  -- newItem <- builderGetObject builder castToMenuItem "new_item"
  -- openItem <- builderGetObject builder castToMenuItem "open_item"
  -- saveItem <- builderGetObject builder castToMenuItem "save_item"
  -- saveAsItem <- builderGetObject builder castToMenuItem "save_as_item"
  -- saveGraphItem <- builderGetObject builder castToMenuItem "save_graph_item"
  -- openGraphItem <- builderGetObject builder castToMenuItem "open_graph_item"
  -- let fileItems = (newItem,openItem,saveItem,saveAsItem,saveGraphItem,openGraphItem)
  --
  -- undoItem <- builderGetObject builder castToMenuItem "undo_item"
  -- redoItem <- builderGetObject builder castToMenuItem "redo_item"
  -- copyItem <- builderGetObject builder castToMenuItem "copy_item"
  -- pasteItem <- builderGetObject builder castToMenuItem "paste_item"
  -- cutItem <- builderGetObject builder castToMenuItem "cut_item"
  -- sallItem <- builderGetObject builder castToMenuItem "sall_item"
  -- snodesItem <- builderGetObject builder castToMenuItem "snodes_item"
  -- sedgesItem <- builderGetObject builder castToMenuItem "sedges_item"
  -- let editItems = (undoItem,redoItem,copyItem,pasteItem,cutItem,sallItem,snodesItem,sedgesItem)
  --
  -- zoomInItem <- builderGetObject builder castToMenuItem "zoomin_item"
  -- zoomOutItem <- builderGetObject builder castToMenuItem "zoomout_item"
  -- zoom50Item <- builderGetObject builder castToMenuItem "zoom50_item"
  -- zoom100Item <- builderGetObject builder castToMenuItem "zoom100_item"
  -- zoom150Item <- builderGetObject builder castToMenuItem "zoom150_item"
  -- zoom200Item <- builderGetObject builder castToMenuItem "zoom200_item"
  -- resetViewItem <- builderGetObject builder castToMenuItem "resetview_item"
  -- let viewItems = (zoomInItem,zoomOutItem,zoom50Item,zoom100Item,zoom150Item,zoom200Item,resetViewItem)
  --
  -- helpItem <- builderGetObject builder castToMenuItem "help_item"
  --
  -- return (menubar, fileItems, editItems, viewItems, helpItem)

-- creates the inspector for typed graphs

buildTypeMenu :: IO Gtk.Frame
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

  return frame
  --
  -- -- creates a HBox containing a label and ColorButton to the user change the node color
  -- hBoxColor <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxColor PackNatural 0
  -- labelColor <- labelNew $ Just "Color: "
  -- boxPackStart hBoxColor labelColor PackNatural 0
  -- colorBtn <- colorButtonNew
  -- boxPackStart hBoxColor colorBtn PackNatural 0
  --
  -- -- creates a HBox containing a label and a ColorButton to the user change the line and text color
  -- hBoxLineColor <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxLineColor PackNatural 0
  -- labelLineColor <- labelNew $ Just "Line Color: "
  -- boxPackStart hBoxLineColor labelLineColor PackNatural 0
  -- lineColorBtn <- colorButtonNew
  -- boxPackStart hBoxLineColor lineColorBtn PackNatural 0
  --
  -- -- creates a frame containing a VBox with radio buttons to the user change the node shape
  -- frameShape <- frameNew
  -- set frameShape [frameLabel := "Node Shape"]
  -- boxPackStart vBoxProps frameShape PackNatural 0
  -- vBoxShape <- vBoxNew False 8
  -- containerAdd frameShape vBoxShape
  -- radioCircle <- radioButtonNewWithLabel "Circle"
  -- boxPackStart vBoxShape radioCircle PackGrow 0
  -- radioRect <- radioButtonNewWithLabelFromWidget radioCircle "Rect"
  -- boxPackStart vBoxShape radioRect PackGrow 0
  -- radioQuad <- radioButtonNewWithLabelFromWidget radioCircle "Quad"
  -- boxPackStart vBoxShape radioQuad PackGrow 0
  -- let radioShapes = [radioCircle, radioRect, radioQuad]

  -- -- creates a frame conataining a VBox with radioButtons to the user change the edge shape
  -- frameStyle <- frameNew
  -- set frameStyle [frameLabel := "Edge Style"]
  -- boxPackStart vBoxProps frameStyle PackNatural 0
  -- vBoxStyle <- vBoxNew False 8
  -- containerAdd frameStyle vBoxStyle
  -- radioNormal <- radioButtonNewWithLabel "Normal"
  -- boxPackStart vBoxStyle radioNormal PackGrow 0
  -- radioPointed <- radioButtonNewWithLabelFromWidget radioNormal "Pointed"
  -- boxPackStart vBoxStyle radioPointed PackGrow 0
  -- radioSlashed <- radioButtonNewWithLabelFromWidget radioNormal "Slashed"
  -- boxPackStart vBoxStyle radioSlashed PackGrow 0
  -- let radioStyles = [radioNormal, radioPointed, radioSlashed]
  --
  -- return (frame, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles, (hBoxColor, frameShape, frameStyle))

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
buildTreePanel :: IO ()
buildTreePanel = do
  return ()
  -- vboxTree <- vBoxNew False 0
  -- treeview <- treeViewNew
  -- boxPackStart vboxTree treeview PackGrow 0
  -- treeViewSetHeadersVisible treeview True
  --
  -- col <- treeViewColumnNew
  -- treeViewColumnSetTitle col "project"
  -- treeViewAppendColumn treeview col
  --
  -- renderer <- cellRendererTextNew
  -- cellLayoutPackStart col renderer False
  -- set renderer [cellTextEditable := True]
  --
  -- btnNew <- buttonNewWithLabel "New Graph"
  -- boxPackStart vboxTree btnNew PackNatural 0
  --
  -- btnRmv <- buttonNewWithLabel "Remove Graph"
  -- boxPackStart vboxTree btnRmv PackNatural 0
  --
  -- return (vboxTree, treeview, renderer, btnNew, btnRmv)



--buildHelpWindow :: IO Gtk.Window
buildHelpWindow :: IO ()
buildHelpWindow = do
  return ()
  -- helpWindow <- windowNew
  -- set helpWindow  [ windowTitle         := "Graph Editor - Help"]
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
  -- return helpWindow


showError :: Maybe Gtk.Window -> String -> IO ()
showError window msg = do
  return ()
  -- dlgE <- messageDialogNew window [DialogDestroyWithParent] MessageError ButtonsOk msg
  -- widgetShow dlgE
  -- dialogRun dlgE
  -- widgetDestroy dlgE
  -- return ()

--createSaveDialog :: Window -> IO FileChooserDialog
createSaveDialog:: Gtk.Window -> IO ()
createSaveDialog window = do
  return ()
  -- saveD <- fileChooserDialogNew
  --          (Just "Salvar arquivo")
  --          (Just window)
  --          FileChooserActionSave
  --          [("Cancela",ResponseCancel),("Salva",ResponseAccept)]
  -- fileChooserSetDoOverwriteConfirmation saveD True
  -- widgetShow saveD
  -- return saveD

createCloseDialog :: Maybe Gtk.Window -> String -> IO Gtk.ResponseType
createCloseDialog window msg = do
  return Gtk.ResponseTypeNo
  --
  -- dlgC <- messageDialogNew window  [DialogDestroyWithParent] MessageWarning ButtonsNone msg
  -- dialogAddButton dlgC "Salvar" ResponseYes
  -- dialogAddButton dlgC "Não Salvar" ResponseNo
  -- dialogAddButton dlgC "Cancelar" ResponseCancel
  -- widgetShow dlgC
  -- response <- dialogRun dlgC
  -- widgetDestroy dlgC
  -- return response
