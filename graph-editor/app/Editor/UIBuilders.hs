-- | This module contains the UI definition
module Editor.UIBuilders
( buildMainWindow
, buildMaybeMenubar
, buildTypeMenu
, buildHostMenu
, buildRuleMenu
, buildTreePanel
, buildHelpWindow
, showError
, createSaveDialog
, createCloseDialog
) where

import Graphics.UI.Gtk
import qualified Data.Text as T

-- builds the main window, containing the treepanel in the left,
-- the canvas in the center,
-- the inspector panel in the right
-- and the menubar in the top of the window
buildMainWindow maybeMenuBar frameProps treePanel = do
  -- main window
  window <- windowNew
  set window  [ windowTitle         := "Graph Editor - UI PROTOTYPE"
              , windowDefaultWidth  := 640
              , windowDefaultHeight := 480]

    -- creates a vBox to separate the editor and the menubar
  vBoxMain <- vBoxNew False 0
  containerAdd window vBoxMain

  -- adds the menubar
  case maybeMenuBar of
    Just x -> boxPackStart vBoxMain x PackNatural 0
    Nothing -> return ()

  -- creates a HPane to add the treeView in the left
  hPaneTree <- hPanedNew
  boxPackStart vBoxMain hPaneTree PackGrow 0
  panedPack1 hPaneTree treePanel False True

  -- creates a HPane to add the canvas in the left and the inspector panel in the right
  hPaneMain <- hPanedNew
  panedPack2 hPaneTree hPaneMain True False
  panedPack2 hPaneMain frameProps False True

  -- creates a frame to englobe the canvas
  frameCanvas <- frameNew
  set frameCanvas [ frameShadowType := ShadowIn ]
  panedPack1 hPaneMain frameCanvas True True
  -- creates a blank canvas
  canvas <- drawingAreaNew
  containerAdd frameCanvas canvas
  widgetSetCanFocus canvas True
  widgetAddEvents canvas [AllEventsMask]
  widgetDelEvents canvas [SmoothScrollMask]
  widgetGrabFocus canvas

  return (window, canvas, hPaneMain)



-- create the menu toolbar
buildMaybeMenubar = do
    fma <- actionNew "FMA" "File" Nothing Nothing
    new <- actionNew "NEW" "New Project" (Just "Just a stub") Nothing
    opn <- actionNew "OPN" "Open Project" (Just "Just a stub") (Just stockOpen)
    svn <- actionNew "SVN" "Save Project" (Just "Just a stub") (Just stockSave)
    sva <- actionNew "SVA" "Save Project As" (Just "Just a stub") (Just stockSaveAs)
    opg <- actionNew "OPG" "Open Graph" (Just "Just a stub") Nothing
    svg <- actionNew "SVG" "Save Graph" (Just "Just a stub") Nothing

    edt <- actionNew "EDT" "Edit" Nothing Nothing
    udo <- actionNew "UDO" "Undo" (Just "Just a stub") Nothing
    rdo <- actionNew "RDO" "Redo" (Just "Just a stub") Nothing
    cpy <- actionNew "CPY" "Copy" (Just "Just a stub") Nothing
    pst <- actionNew "PST" "Paste" (Just "Just a stub") Nothing
    cut <- actionNew "CUT" "Cut" (Just "Just a stub") Nothing
    sla <- actionNew "SLA" "Select All" (Just "Just a stub") Nothing
    sle <- actionNew "SLE" "Select Edges" (Just "Just a stub") Nothing
    sln <- actionNew "SLN" "Select Nodes" (Just "Just a stub") Nothing

    hlp <- actionNew "HLP" "Help" (Just "Just a stub") Nothing
    hlp' <- actionNew "HLP'" "Help" (Just "Just a stub") Nothing
    agr <- actionGroupNew "AGR"
    mapM_ (actionGroupAddAction agr) [fma,edt,hlp]
    mapM_ (\act -> actionGroupAddActionWithAccel agr act (Nothing :: Maybe String)) [new,opn,svn,sva,opg,svg,udo,rdo,cpy,pst,cut,sla,sle,sln,hlp']

    ui <- uiManagerNew
    uiManagerAddUiFromString ui uiStr
    uiManagerInsertActionGroup ui agr 0
    maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
    return (maybeMenubar, new, opn, svn, sva, opg, svg, udo, rdo, cpy, pst, cut, sla, sle, sln, hlp')

  where uiStr = "<ui>\
\                 <menubar>\
\                   <menu action=\"FMA\">\
\                     <menuitem action=\"NEW\"/>\
\                     <menuitem action=\"OPN\"/>\
\                     <menuitem action=\"SVN\"/>\
\                     <menuitem action=\"SVA\"/>\
\                     <separator/>\
\                     <menuitem action=\"SVG\"/>\
\                     <menuitem action=\"OPG\"/>\
\                   </menu> \
\                   <menu action=\"EDT\">\
\                     <menuitem action=\"UDO\"/>\
\                     <menuitem action=\"RDO\"/>\
\                     <separator/>\
\                     <menuitem action=\"CPY\"/>\
\                     <menuitem action=\"PST\"/>\
\                     <menuitem action=\"CUT\"/>\
\                     <menuitem action=\"SLA\"/>\
\                     <menuitem action=\"SLE\"/>\
\                     <menuitem action=\"SLN\"/>\
\                   </menu>\
\                   <menu action=\"HLP\">\
\                     <menuitem action=\"HLP'\"/>\
\                   </menu>\
\                 </menubar>\
\                </ui>"


-- creates the inspector for typed graphs
buildTypeMenu = do
  frame <- frameNew
  set frame [ frameShadowType := ShadowIn ]

  vBoxProps <- vBoxNew False 8
  containerAdd frame vBoxProps

  -- creates the title label
  titleLabel <- labelNew $ Just "Inspector"
  boxPackStart vBoxProps titleLabel PackNatural 0

  -- creates a HBox containing a label and a entry for the user change the type name
  hBoxName <- hBoxNew False 8
  boxPackStart vBoxProps hBoxName PackNatural 0
  labelName <- labelNew $ Just "Label: "
  boxPackStart hBoxName labelName PackNatural 0
  entryName <- entryNew
  boxPackStart hBoxName entryName PackGrow 0
  widgetSetCanFocus entryName True

  -- creates a HBox containing a label and ColorButton to the user change the node color
  hBoxColor <- hBoxNew False 8
  boxPackStart vBoxProps hBoxColor PackNatural 0
  labelColor <- labelNew $ Just "Color: "
  boxPackStart hBoxColor labelColor PackNatural 0
  colorBtn <- colorButtonNew
  boxPackStart hBoxColor colorBtn PackNatural 0

  -- creates a HBox containing a label and a ColorButton to the user change the line and text color
  hBoxLineColor <- hBoxNew False 8
  boxPackStart vBoxProps hBoxLineColor PackNatural 0
  labelLineColor <- labelNew $ Just "Line Color: "
  boxPackStart hBoxLineColor labelLineColor PackNatural 0
  lineColorBtn <- colorButtonNew
  boxPackStart hBoxLineColor lineColorBtn PackNatural 0

  -- creates a frame containing a VBox with radio buttons to the user change the node shape
  frameShape <- frameNew
  set frameShape [frameLabel := "Node Shape"]
  boxPackStart vBoxProps frameShape PackNatural 0
  vBoxShape <- vBoxNew False 8
  containerAdd frameShape vBoxShape
  radioCircle <- radioButtonNewWithLabel "Circle"
  boxPackStart vBoxShape radioCircle PackGrow 0
  radioRect <- radioButtonNewWithLabelFromWidget radioCircle "Rect"
  boxPackStart vBoxShape radioRect PackGrow 0
  radioQuad <- radioButtonNewWithLabelFromWidget radioCircle "Quad"
  boxPackStart vBoxShape radioQuad PackGrow 0
  let radioShapes = [radioCircle, radioRect, radioQuad]

  -- creates a frame conataining a VBox with radioButtons to the user change the edge shape
  frameStyle <- frameNew
  set frameStyle [frameLabel := "Edge Style"]
  boxPackStart vBoxProps frameStyle PackNatural 0
  vBoxStyle <- vBoxNew False 8
  containerAdd frameStyle vBoxStyle
  radioNormal <- radioButtonNewWithLabel "Normal"
  boxPackStart vBoxStyle radioNormal PackGrow 0
  radioPointed <- radioButtonNewWithLabelFromWidget radioNormal "Pointed"
  boxPackStart vBoxStyle radioPointed PackGrow 0
  radioSlashed <- radioButtonNewWithLabelFromWidget radioNormal "Slashed"
  boxPackStart vBoxStyle radioSlashed PackGrow 0
  let radioStyles = [radioNormal, radioPointed, radioSlashed]

  return (frame, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles, (hBoxColor, frameShape, frameStyle))

-- creates the inspector for the host graph
buildHostMenu = do
  frame <- frameNew
  set frame [ frameShadowType := ShadowIn ]

  vBoxProps <- vBoxNew False 8
  containerAdd frame vBoxProps

  -- creates a title label
  titleLabel <- labelNew $ Just "Inspector"
  boxPackStart vBoxProps titleLabel PackNatural 0

  -- creates a HBox containing a entry for the user change the node label
  hBoxLabel <- hBoxNew False 8
  boxPackStart vBoxProps hBoxLabel PackNatural 0
  labelLabel <- labelNew $ Just "Label: "
  entryLabel <- entryNew
  boxPackStart hBoxLabel labelLabel PackNatural 0
  boxPackStart hBoxLabel entryLabel PackGrow 0
  widgetSetCanFocus entryLabel True

  -- creates a HBox containing a ComboBox for the user change the node type
  hBoxNodeType <- hBoxNew False 8
  boxPackStart vBoxProps hBoxNodeType PackNatural 0
  labelNodeType <- labelNew $ Just "Node Type: "
  boxPackStart hBoxNodeType labelNodeType PackNatural 0
  comboBoxNodeType <- comboBoxNewText
  boxPackStart hBoxNodeType comboBoxNodeType PackGrow 0

  -- creates a HBox conataining a ComboBox for the user change the edge type
  hBoxEdgeType <- hBoxNew False 8
  boxPackStart vBoxProps hBoxEdgeType PackNatural 0
  labelEdgeType <- labelNew $ Just "Edge Type: "
  boxPackStart hBoxEdgeType labelEdgeType PackNatural 0
  comboBoxEdgeType <- comboBoxNewText
  boxPackStart hBoxEdgeType comboBoxEdgeType PackGrow 0

  return (frame, entryLabel, comboBoxNodeType, comboBoxEdgeType, (hBoxNodeType, hBoxEdgeType))

buildRuleMenu = do
  frame <- frameNew
  set frame [ frameShadowType := ShadowIn ]

  vBoxProps <- vBoxNew False 8
  containerAdd frame vBoxProps

  -- creates the title label
  titleLabel <- labelNew $ Just "Inspector"
  boxPackStart vBoxProps titleLabel PackNatural 0

  -- creates a HBox containing a entry for the user change the node label
  hBoxLabel <- hBoxNew False 8
  boxPackStart vBoxProps hBoxLabel PackNatural 0
  labelLabel <- labelNew $ Just "Label: "
  entryLabel <- entryNew
  boxPackStart hBoxLabel labelLabel PackNatural 0
  boxPackStart hBoxLabel entryLabel PackGrow 0
  widgetSetCanFocus entryLabel True

  -- creates a HBox containing a ComboBox for the user change the node type
  hBoxNodeType <- hBoxNew False 8
  boxPackStart vBoxProps hBoxNodeType PackNatural 0
  labelNodeType <- labelNew $ Just "Node Type: "
  boxPackStart hBoxNodeType labelNodeType PackNatural 0
  comboBoxNodeType <- comboBoxNewText
  boxPackStart hBoxNodeType comboBoxNodeType PackGrow 0

  -- creates a HBox containing a ComboBox for the user change the edge type
  hBoxEdgeType <- hBoxNew False 8
  boxPackStart vBoxProps hBoxEdgeType PackNatural 0
  labelEdgeType <- labelNew $ Just "Edge Type: "
  boxPackStart hBoxEdgeType labelEdgeType PackNatural 0
  comboBoxEdgeType <- comboBoxNewText
  boxPackStart hBoxEdgeType comboBoxEdgeType PackGrow 0

  -- creates a HBox containing a ComboBox for the user change the operation to be applyed in the graph element
  hBoxOperation <- hBoxNew False 8
  boxPackStart vBoxProps hBoxOperation PackNatural 0
  labelOperation <- labelNew $ Just "Operation: "
  boxPackStart hBoxOperation labelOperation PackNatural 0
  comboBoxOperation <- comboBoxNewText
  boxPackStart hBoxOperation comboBoxOperation PackGrow 0

  return (frame, entryLabel, comboBoxNodeType, comboBoxEdgeType, comboBoxOperation, (hBoxNodeType, hBoxEdgeType))

-- creates the treePanel
buildTreePanel = do
  vboxTree <- vBoxNew False 0
  treeview <- treeViewNew
  boxPackStart vboxTree treeview PackGrow 0
  treeViewSetHeadersVisible treeview True

  col <- treeViewColumnNew
  treeViewColumnSetTitle col "project"
  treeViewAppendColumn treeview col

  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer False
  set renderer [cellTextEditable := True]

  btnNew <- buttonNewWithLabel "New Graph"
  boxPackStart vboxTree btnNew PackNatural 0

  btnRmv <- buttonNewWithLabel "Remove Graph"
  boxPackStart vboxTree btnRmv PackNatural 0

  return (vboxTree, treeview, renderer, btnNew, btnRmv)



buildHelpWindow :: IO Window
buildHelpWindow = do
  helpWindow <- windowNew
  set helpWindow  [ windowTitle         := "Graph Editor - Help"]
  helpBuffer <- textBufferNew Nothing
  textBufferInsertAtCursor helpBuffer "<Creating and selecting>: \n"
  textBufferInsertAtCursor helpBuffer "Click with the right mouse button in a blank space to create a new node. \n"
  textBufferInsertAtCursor helpBuffer "Click with the left mouse button in a node/edge to select it. \n"
  textBufferInsertAtCursor helpBuffer "Click with the left button in a node/edge while the Shift key is pressed to add it to the selection. \n"
  textBufferInsertAtCursor helpBuffer "Click with the left button in a node/edge while the Shift and Ctrl keys are pressed to remove it from the selection. \n"
  textBufferInsertAtCursor helpBuffer "Click with the right button in a node while there's other nodes selected to create edges from the selected nodes to it. \n"
  textBufferInsertAtCursor helpBuffer "<Changing the node properties>: \n"
  textBufferInsertAtCursor helpBuffer "To change the properties of a node/edge, select it and use the inspector on the right. \n"
  textBufferInsertAtCursor helpBuffer "Double-clicking a node or edge, or pressing F2 will focus on the name entry box on the inspector panel. \n"
  textBufferInsertAtCursor helpBuffer "<zoom and navigation>: \n"
  textBufferInsertAtCursor helpBuffer "Use Ctrl + mouse wheel or Ctrl + [+/-] to change the zoom level. \n"
  textBufferInsertAtCursor helpBuffer "Use Ctrl + [=] to change the zoom level to the original. \n"
  textBufferInsertAtCursor helpBuffer "Hold the middle mouse button, or Ctrl + right mouse button, to navigate throught the canvas. \n"
  textBufferInsertAtCursor helpBuffer "Press ctrl + 0 to return to the initial position of the canvas and reset the zoom to the original. \n"
  helpView <- textViewNewWithBuffer helpBuffer
  containerAdd helpWindow helpView

  return helpWindow


showError :: Maybe Window -> String -> IO ()
showError window msg = do
  dlgE <- messageDialogNew window [DialogDestroyWithParent] MessageError ButtonsOk msg
  widgetShow dlgE
  dialogRun dlgE
  widgetDestroy dlgE
  return ()

createSaveDialog :: Window -> IO FileChooserDialog
createSaveDialog window = do
  saveD <- fileChooserDialogNew
           (Just "Salvar arquivo")
           (Just window)
           FileChooserActionSave
           [("Cancela",ResponseCancel),("Salva",ResponseAccept)]
  fileChooserSetDoOverwriteConfirmation saveD True
  widgetShow saveD
  return saveD

createCloseDialog :: Maybe Window -> String -> IO ResponseId
createCloseDialog window msg = do
  dlgC <- messageDialogNew window  [DialogDestroyWithParent] MessageWarning ButtonsNone msg
  dialogAddButton dlgC "Salvar" ResponseYes
  dialogAddButton dlgC "Não Salvar" ResponseNo
  dialogAddButton dlgC "Cancelar" ResponseCancel
  widgetShow dlgC
  response <- dialogRun dlgC
  widgetDestroy dlgC
  return response
