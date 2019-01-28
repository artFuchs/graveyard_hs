-- | Esse modulo contem a definição dos widgets da interface do usuario
module UIConstructors
( buildMaybeMenubar
, buildPropMenu
) where

import Graphics.UI.Gtk hiding (rectangle)

-- constroi a menu toolbar
buildMaybeMenubar = do
    fma <- actionNew "FMA" "File" Nothing Nothing
    opn <- actionNew "OPN" "Open File" (Just "Just a stub") (Just stockOpen)
    svn <- actionNew "SVN" "Save File" (Just "Just a stub") (Just stockSave)
    edt <- actionNew "EDT" "Edit" Nothing Nothing
    udo <- actionNew "UDO" "Undo" (Just "Just a stub") (Just stockUndo)
    rdo <- actionNew "RDO" "Redo" (Just "Just a stub") (Just stockRedo)
    agr <- actionGroupNew "AGR"
    mapM_ (actionGroupAddAction agr) [fma,edt]
    mapM_ (\act -> actionGroupAddActionWithAccel agr act (Nothing :: Maybe String)) [opn,svn,udo,rdo]

    ui <- uiManagerNew
    uiManagerAddUiFromString ui uiStr
    uiManagerInsertActionGroup ui agr 0
    maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
    return (maybeMenubar, opn, svn, udo, rdo)

  where uiStr = "<ui>\
\                 <menubar>\
\                   <menu action=\"FMA\">\
\                     <menuitem action=\"OPN\"/>\
\                     <menuitem action=\"SVN\"/>\
\                   </menu> \
\                   <menu action=\"EDT\">\
\                     <menuitem action=\"UDO\"/>\
\                     <menuitem action=\"RDO\"/>\
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

  -- cria um frame contendo uma VBox para a propriedade forma do nodo
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

  return (frame, entryID, entryName, colorBtn, lineColorBtn, radioShapes, (hBoxColor, frameShape))
