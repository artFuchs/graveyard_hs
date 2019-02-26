import Graphics.UI.Gtk
import Data.Tree
import Control.Monad.IO.Class

main = do
  initGUI

  window <- windowNew
  set window  [ windowTitle         := "Tree test"
              , windowResizable     := True]

  box <- vBoxNew False 0
  containerAdd window box

  let myTree = Node "games" [Node "Left4Dead" [], Node "Street Fighter" []]
  let myTree2 = Node "office" [Node "Writer" [], Node "Math" [], Node "Presentation" []]
  store <- treeStoreNew [myTree, myTree2]

  treeview <- treeViewNewWithModel store
  treeViewSetHeadersVisible treeview True
  boxPackStart box treeview PackGrow 4

  col <- treeViewColumnNew
  treeViewColumnSetTitle col "tree"
  renderer <- cellRendererTextNew
  set renderer  [ cellTextEditable := True ]
  cellLayoutPackStart col renderer False
  cellLayoutSetAttributes col renderer store $ \ind -> [cellText := ind]
  treeViewAppendColumn treeview col

  addMBtn <- buttonNewWithLabel "Add Menu"
  boxPackStart box addMBtn PackNatural 0
  addIBtn <- buttonNewWithLabel "Add Item"
  boxPackStart box addIBtn PackNatural 0
  removeBtn <- buttonNewWithLabel "Remove Selected"
  boxPackStart box removeBtn PackNatural 0


  -- events
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  renderer `on` edited $ \path newText -> do
    treeStoreChange store path (\_ -> newText)
    return ()

  addMBtn `on` buttonActivated $ do
    n <- (treeModelIterNChildren store Nothing)
    treeStoreInsert store [] n "newMenu"

  addIBtn `on` buttonActivated $ do
    selection <- treeViewGetSelection treeview
    sel <- treeSelectionGetSelected selection
    case sel of
      Nothing -> return ()
      Just it -> do
        path <- treeModelGetPath store it
        n <- treeModelIterNChildren store sel
        treeStoreInsert store path n "newItem"
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




  -- show things
  widgetShowAll window
  mainGUI
