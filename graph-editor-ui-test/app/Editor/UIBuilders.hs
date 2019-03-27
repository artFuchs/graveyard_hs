-- | Esse modulo contem a definição dos widgets da interface do usuario
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

buildMainWindow maybeMenuBar frameProps treePanel = do
  -- janela principal
  window <- windowNew
  set window  [ windowTitle         := "Graph Editor - UI PROTOTYPE"
              , windowDefaultWidth  := 640
              , windowDefaultHeight := 480]

  -- cria uma VBox para separar o editor do menu
  vBoxMain <- vBoxNew False 0
  containerAdd window vBoxMain

  -- adiciona o menu
  case maybeMenuBar of
    Just x -> boxPackStart vBoxMain x PackNatural 0
    Nothing -> return ()

  -- cria um HPane para dividir a arvore de grafos e o editor
  hPaneTree <- hPanedNew
  boxPackStart vBoxMain hPaneTree PackGrow 0
  panedPack1 hPaneTree treePanel False True

  -- cria um HPane para dividir o canvas e o menu de propriedades
  hPaneMain <- hPanedNew
  panedPack2 hPaneTree hPaneMain True False
  panedPack2 hPaneMain frameProps False True

  -- cria um frame para englobar o canvas
  frameCanvas <- frameNew
  set frameCanvas [ frameShadowType := ShadowIn ]
  -- adiciona o frame do Canvas
  panedPack1 hPaneMain frameCanvas True True
  -- cria um canvas em branco
  canvas <- drawingAreaNew
  containerAdd frameCanvas canvas
  widgetSetCanFocus canvas True
  widgetAddEvents canvas [AllEventsMask]
  widgetDelEvents canvas [SmoothScrollMask]
  widgetGrabFocus canvas

  return (window, canvas, hPaneMain)



-- constroi a menu toolbar
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


-- constroi o painel do menu de propriedades para grafos tipados
buildTypeMenu = do
  frame <- frameNew
  set frame [ frameShadowType := ShadowIn ]

  vBoxProps <- vBoxNew False 8
  containerAdd frame vBoxProps

  -- cria a label de titulo
  titleLabel <- labelNew $ Just "Inspector"
  boxPackStart vBoxProps titleLabel PackNatural 0

  -- cria uma HBox para a propriedade nome
  hBoxName <- hBoxNew False 8
  boxPackStart vBoxProps hBoxName PackNatural 0
  labelName <- labelNew $ Just "Type name: "
  boxPackStart hBoxName labelName PackNatural 0
  entryName <- entryNew
  boxPackStart hBoxName entryName PackGrow 0
  widgetSetCanFocus entryName True

  -- cria uma HBox para a propriedade cor
  hBoxColor <- hBoxNew False 8
  boxPackStart vBoxProps hBoxColor PackNatural 0
  labelColor <- labelNew $ Just "Color: "
  boxPackStart hBoxColor labelColor PackNatural 0
  colorBtn <- colorButtonNew
  boxPackStart hBoxColor colorBtn PackNatural 0

  -- cria uma HBox para a propriedade cor da linha
  hBoxLineColor <- hBoxNew False 8
  boxPackStart vBoxProps hBoxLineColor PackNatural 0
  labelLineColor <- labelNew $ Just "Line Color: "
  boxPackStart hBoxLineColor labelLineColor PackNatural 0
  lineColorBtn <- colorButtonNew
  boxPackStart hBoxLineColor lineColorBtn PackNatural 0

  -- cria um frame contendo uma VBox para a propriedade Node Shape
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

  -- cria um frame contendo uma VBox para a propriedade Edge Style
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

buildHostMenu = do
  frame <- frameNew
  set frame [ frameShadowType := ShadowIn ]

  vBoxProps <- vBoxNew False 8
  containerAdd frame vBoxProps

  -- cria a label de titulo
  titleLabel <- labelNew $ Just "Inspector"
  boxPackStart vBoxProps titleLabel PackNatural 0

  -- cria uma HBox para a propriedade nome
  hBoxName <- hBoxNew False 8
  boxPackStart vBoxProps hBoxName PackNatural 0
  labelName <- labelNew $ Just "Name: "
  entryName <- entryNew
  boxPackStart hBoxName labelName PackNatural 0
  boxPackStart hBoxName entryName PackGrow 0
  widgetSetCanFocus entryName True

  -- cria uma HBox para a propriedade Tipo do nodo
  hBoxNodeType <- hBoxNew False 8
  boxPackStart vBoxProps hBoxNodeType PackNatural 0
  labelNodeType <- labelNew $ Just "Node Type: "
  boxPackStart hBoxNodeType labelNodeType PackNatural 0
  comboBoxNodeType <- comboBoxNewText
  boxPackStart hBoxNodeType comboBoxNodeType PackGrow 0

  -- cria uma HBOx para a propriedade Tipo da edge
  hBoxEdgeType <- hBoxNew False 8
  boxPackStart vBoxProps hBoxEdgeType PackNatural 0
  labelEdgeType <- labelNew $ Just "Edge Type: "
  boxPackStart hBoxEdgeType labelEdgeType PackNatural 0
  comboBoxEdgeType <- comboBoxNewText
  boxPackStart hBoxEdgeType comboBoxEdgeType PackGrow 0

  return (frame, entryName, comboBoxNodeType, comboBoxEdgeType, (hBoxNodeType, hBoxEdgeType))

buildRuleMenu = do
  frame <- frameNew
  set frame [ frameShadowType := ShadowIn ]

  vBoxProps <- vBoxNew False 8
  containerAdd frame vBoxProps

  -- cria a label de titulo
  titleLabel <- labelNew $ Just "Inspect"
  boxPackStart vBoxProps titleLabel PackNatural 0

  -- cria uma HBox para a propriedade nome
  hBoxName <- hBoxNew False 8
  boxPackStart vBoxProps hBoxName PackNatural 0
  labelName <- labelNew $ Just "Name: "
  entryName <- entryNew
  boxPackStart hBoxName labelName PackNatural 0
  boxPackStart hBoxName entryName PackGrow 0
  widgetSetCanFocus entryName True

  -- cria uma HBox para a propriedade Tipo do nodo
  hBoxNodeType <- hBoxNew False 8
  boxPackStart vBoxProps hBoxNodeType PackNatural 0
  labelNodeType <- labelNew $ Just "Node Type: "
  boxPackStart hBoxNodeType labelNodeType PackNatural 0
  comboBoxNodeType <- comboBoxNewText
  boxPackStart hBoxNodeType comboBoxNodeType PackGrow 0

  -- cria uma HBOx para a propriedade Tipo da edge
  hBoxEdgeType <- hBoxNew False 8
  boxPackStart vBoxProps hBoxEdgeType PackNatural 0
  labelEdgeType <- labelNew $ Just "Edge Type: "
  boxPackStart hBoxEdgeType labelEdgeType PackNatural 0
  comboBoxEdgeType <- comboBoxNewText
  boxPackStart hBoxEdgeType comboBoxEdgeType PackGrow 0

  -- cria uma HBox para a propriedade Operação
  hBoxOperation <- hBoxNew False 8
  boxPackStart vBoxProps hBoxOperation PackNatural 0
  labelOperation <- labelNew $ Just "Operation: "
  boxPackStart hBoxOperation labelOperation PackNatural 0
  comboBoxOperation <- comboBoxNewText
  boxPackStart hBoxOperation comboBoxOperation PackGrow 0

  return (frame, entryName, comboBoxNodeType, comboBoxEdgeType, comboBoxOperation, (hBoxNodeType, hBoxEdgeType))


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
  textBufferInsertAtCursor helpBuffer "Instruções: \n"
  textBufferInsertAtCursor helpBuffer "Clique com o botão direito do mouse no espaço vazio para criar um novo nodo. \n"
  textBufferInsertAtCursor helpBuffer "Clique com o botão esquerdo sobre um nodo/aresta para seleciona-lo(a). \n"
  textBufferInsertAtCursor helpBuffer "Clique com o botão esquerdo sobre um nodo/aresta + Shift para adiciona-lo à seleção. \n"
  textBufferInsertAtCursor helpBuffer "Clique com o botão esquerdo sobre um nodo/aresta + Shift + Ctrl para removê-lo da seleção. \n"
  textBufferInsertAtCursor helpBuffer "Clique com o botão esquerdo sobre um espaço vazio e arraste o mouse para criar uma àrea de seleção. Os nodos/arestas que estiverem dentro dessa área serão selecionados. \n"
  textBufferInsertAtCursor helpBuffer "Clique com o botão direito sobre um nodo para criar arestas dos nodos selecionados para ele. \n"
  textBufferInsertAtCursor helpBuffer "Para modificar as propriedades de um nodo/aresta, selecione-o e utiliza o menu de propriedades à direita. \n"
  textBufferInsertAtCursor helpBuffer "Use Ctrl + roda do mouse ou Ctrl + [+/-] para aumentar/reduzir o zoom. \n"
  textBufferInsertAtCursor helpBuffer "Use Ctrl + [=] para restaurar o zoom para o original. \n"
  textBufferInsertAtCursor helpBuffer "Pressione o botão do meio do mouse, ou Ctrl + botão direito do mouse para navegar pelo canvas. \n"

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
