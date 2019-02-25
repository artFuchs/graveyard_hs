-- | Esse modulo contem a definição dos widgets da interface do usuario
module Editor.UIBuilders
( buildMainWindow
, buildMaybeMenubar
, buildPropMenu
, buildHelpWindow
, showError
) where

import Graphics.UI.Gtk
import qualified Data.Text as T

buildMainWindow maybeMenuBar frameProps = do
  -- janela principal
  window <- windowNew
  set window  [ windowTitle         := "Graph Editor"
              , windowDefaultWidth  := 640
              , windowDefaultHeight := 480]

  -- cria uma VBox para separar o editor do menu
  vBoxMain <- vBoxNew False 0
  containerAdd window vBoxMain

  -- adiciona o menu
  case maybeMenuBar of
    Just x -> boxPackStart vBoxMain x PackNatural 0
    Nothing -> return ()

  -- cria um HPane para dividir o canvas e o menu de propriedades
  hPaneAction <- hPanedNew
  boxPackStart vBoxMain hPaneAction PackGrow 0
  -- adiciona o menu de propriedades à UI
  panedPack2 hPaneAction frameProps False True

  -- cria um frame para englobar o canvas
  frameCanvas <- frameNew
  set frameCanvas [ frameShadowType := ShadowIn ]
  -- adiciona o frame do Canvas
  panedPack1 hPaneAction frameCanvas True True
  -- cria um canvas em branco
  canvas <- drawingAreaNew
  containerAdd frameCanvas canvas
  widgetSetCanFocus canvas True
  widgetAddEvents canvas [AllEventsMask]
  widgetDelEvents canvas [SmoothScrollMask]
  widgetGrabFocus canvas

  return (window, canvas)



-- constroi a menu toolbar
buildMaybeMenubar = do
    fma <- actionNew "FMA" "File" Nothing Nothing
    new <- actionNew "NEW" "New File" (Just "Just a stub") Nothing
    opn <- actionNew "OPN" "Open File" (Just "Just a stub") (Just stockOpen)
    svn <- actionNew "SVN" "Save File" (Just "Just a stub") (Just stockSave)
    sva <- actionNew "SVA" "Save File As" (Just "Just a stub") (Just stockSaveAs)
    edt <- actionNew "EDT" "Edit" Nothing Nothing
    udo <- actionNew "UDO" "Undo" (Just "Just a stub") Nothing
    rdo <- actionNew "RDO" "Redo" (Just "Just a stub") Nothing
    hlp <- actionNew "HLP" "Help" (Just "Just a stub") Nothing
    hlp' <- actionNew "HLP'" "Help" (Just "Just a stub") Nothing
    agr <- actionGroupNew "AGR"
    mapM_ (actionGroupAddAction agr) [fma,edt,hlp]
    mapM_ (\act -> actionGroupAddActionWithAccel agr act (Nothing :: Maybe String)) [new,opn,svn,sva,udo,rdo,hlp']

    ui <- uiManagerNew
    uiManagerAddUiFromString ui uiStr
    uiManagerInsertActionGroup ui agr 0
    maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
    return (maybeMenubar, new, opn, svn, sva, udo, rdo, hlp')

  where uiStr = "<ui>\
\                 <menubar>\
\                   <menu action=\"FMA\">\
\                     <menuitem action=\"NEW\"/>\
\                     <menuitem action=\"OPN\"/>\
\                     <menuitem action=\"SVN\"/>\
\                     <menuitem action=\"SVA\"/>\
\                   </menu> \
\                   <menu action=\"EDT\">\
\                     <menuitem action=\"UDO\"/>\
\                     <menuitem action=\"RDO\"/>\
\                   </menu>\
\                   <menu action=\"HLP\">\
\                     <menuitem action=\"HLP'\"/>\
\                   </menu>\
\                 </menubar>\
\                </ui>"


-- constroi o painel do menu de propriedades
buildPropMenu = do
  frame <- frameNew
  set frame [ frameShadowType := ShadowIn ]

  vBoxProps <- vBoxNew False 8
  containerAdd frame vBoxProps

  -- cria a label de titulo
  titleLabel <- labelNew $ Just "Propriedades"
  boxPackStart vBoxProps titleLabel PackNatural 0

  -- cria uma HBox para a propriedade ID
  hBoxID <- hBoxNew False 8
  boxPackStart vBoxProps hBoxID PackNatural 0
  labelID <- labelNew $ Just "ID: "
  boxPackStart hBoxID labelID PackNatural 0
  entryID <- entryNew
  boxPackStart hBoxID entryID PackGrow 0
  widgetSetCanFocus entryID False
  set entryID [ entryEditable := False ]

  -- cria uma HBox para a propriedade nome
  hBoxName <- hBoxNew False 8
  boxPackStart vBoxProps hBoxName PackNatural 0
  labelName <- labelNew $ Just "Nome: "
  boxPackStart hBoxName labelName PackNatural 0
  entryName <- entryNew
  boxPackStart hBoxName entryName PackGrow 0
  widgetSetCanFocus entryName True

  -- cria uma HBox para a propriedade cor
  hBoxColor <- hBoxNew False 8
  boxPackStart vBoxProps hBoxColor PackNatural 0
  labelColor <- labelNew $ Just "Cor: "
  boxPackStart hBoxColor labelColor PackNatural 0
  colorBtn <- colorButtonNew
  boxPackStart hBoxColor colorBtn PackNatural 0

  -- cria uma HBox para a propriedade cor da linha
  hBoxLineColor <- hBoxNew False 8
  boxPackStart vBoxProps hBoxLineColor PackNatural 0
  labelLineColor <- labelNew $ Just "Cor da linha: "
  boxPackStart hBoxLineColor labelLineColor PackNatural 0
  lineColorBtn <- colorButtonNew
  boxPackStart hBoxLineColor lineColorBtn PackNatural 0

  -- cria um frame contendo uma VBox para a propriedade Node Shape
  frameShape <- frameNew
  set frameShape [frameLabel := "Node Shape"]
  boxPackStart vBoxProps frameShape PackNatural 0
  vBoxShape <- vBoxNew False 8
  containerAdd frameShape vBoxShape
  radioCircle <- radioButtonNewWithLabel "Circulo"
  boxPackStart vBoxShape radioCircle PackGrow 0
  radioRect <- radioButtonNewWithLabelFromWidget radioCircle "Retangulo"
  boxPackStart vBoxShape radioRect PackGrow 0
  radioQuad <- radioButtonNewWithLabelFromWidget radioCircle "Quadrado"
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

  return (frame, entryID, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles, (hBoxColor, frameShape, frameStyle))

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
