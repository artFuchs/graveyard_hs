-- A simple application to play with the treeView widget
-- It displays a "book", with the document sections available on the left and
-- contents of the selected section being displayed at the right

import Graphics.UI.Gtk
import Data.Tree
import Control.Monad.IO.Class

main = do

  -- content of the book
  let intro = Node ("Introduction","This is the introduction of this beautiful book. \
                                    \\nSelect the chapters in the tree at the left") []
  let part1 = Node ("Part 1", "The cake")
                   [ Node ("Chapter 1", "Onde upon a time, there was a girl.") []
                   , Node ("Chapter 2", "The girl liked cake.") []
                   , Node ("chapter 3", "The cake was a lie.") []
                   ]
  let part2 = Node ("Part 2", "The hot chocolate")
                   [ Node ("Chapter 1", "Onde upon a time, there was a boy.") []
                   , Node ("Chapter 2", "The boy liked hot chocolate.") []
                   , Node ("chapter 3", "The hot chocolate was too hot.") []
                   ]

  initGUI
  -- create the window
  window <- windowNew
  set window  [ windowTitle         := "A beautiful book"
              , windowResizable     := True
              , windowDefaultWidth  := 500]

  -- create the mainpaned, an hBox that divides the tree from the content
  mainpane <- hPanedNew
  containerAdd window mainpane

  -- create the  content area
  labelbook <- labelNew $ Just ""
  labelSetLineWrap labelbook True
  panedPack2 mainpane labelbook True False

  -- create the tree area
  treebox <- vBoxNew False 0
  panedPack1 mainpane treebox False False

  treescroll <- scrolledWindowNew Nothing Nothing
  boxPackStart treebox treescroll PackGrow 4


  -- create the treeview
  store <- treeStoreNew [intro,part1,part2]
  treeview <- treeViewNewWithModel store
  treeViewSetHeadersVisible treeview True
  containerAdd treescroll treeview

  col <- treeViewColumnNew
  treeViewColumnSetTitle col "tree"
  renderer <- cellRendererTextNew
  set renderer  [ cellTextEditable := True ]
  cellLayoutPackStart col renderer False
  cellLayoutSetAttributes col renderer store $ \ind -> [cellText := fst ind]
  treeViewAppendColumn treeview col

  addMBtn <- buttonNewWithLabel "Add Part"
  boxPackStart treebox addMBtn PackNatural 0
  addIBtn <- buttonNewWithLabel "Add Chapter"
  boxPackStart treebox addIBtn PackNatural 0
  removeBtn <- buttonNewWithLabel "Remove Selected"
  boxPackStart treebox removeBtn PackNatural 0


  -- events
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  renderer `on` editingStarted $ \widget path -> do
    let entry = castToEntry widget
    entry `on` entryPopulatePopup $ \menu -> do
      items <- containerGetChildren menu
      containerRemove menu (items!!(length items -1))
      menuItem <- menuItemNewWithLabel "Do nothing"
      menuShellAppend menu menuItem
      widgetShowAll menu
      return ()
    return ()

  renderer `on` edited $ \path newName -> do
    treeStoreChange store path (\(_,text) -> (newName,text))
    return ()

  addMBtn `on` buttonActivated $ do
    n <- (treeModelIterNChildren store Nothing)
    treeStoreInsert store [] n ("newPart","")

  addIBtn `on` buttonActivated $ do
    selection <- treeViewGetSelection treeview
    sel <- treeSelectionGetSelected selection
    case sel of
      Nothing -> return ()
      Just it -> do
        path <- treeModelGetPath store it
        n <- treeModelIterNChildren store sel
        treeStoreInsert store path n ("newChapter","")
    return ()

  removeBtn `on` buttonActivated $ do
    selection <- treeViewGetSelection treeview
    sel <- treeSelectionGetSelected selection
    case sel of
      Nothing -> return ()
      Just it -> do
        path <- treeModelGetPath store it
        treeStoreRemove store path
        return ()

  treeview `on` cursorChanged $ do
    selection <- treeViewGetSelection treeview
    sel <- treeSelectionGetSelected selection
    case sel of
      Nothing -> return ()
      Just it -> do
        path <- treeModelGetPath store it
        (_,text) <- treeStoreGetValue store path
        labelSetText labelbook text


  -- show things
  widgetShowAll window
  mainGUI
