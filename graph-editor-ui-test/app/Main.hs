-- essa é uma ideia de interface para a expansão do projeto graph-editor para um editor de gramática de grafo.
-- meramente um protótipo, sem incluir as funcionalidades do editor

module Main where

import Graphics.UI.Gtk hiding (rectangle)
import Control.Monad.IO.Class
import Data.Tree
import qualified Data.Text as T
import Editor.UIBuilders

type Info = String
data GraphType = MenuGraph | HostGraph Info | TypeGraph Info | RuleGraph Info deriving (Show)
type Name = String
data GraphStore = Store GraphType Name


main = do
  initGUI

  (frameH, entryName, comboBoxNodeType, comboBoxEdgeType, (hBoxNodeType, hBoxEdgeType)) <- buildHostMenu
  (frameT, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles, (hBoxColor, frameShape, frameStyle)) <- buildTypeMenu
  let propMenuFrames = (frameH, frameT)
  (maybeMenubar, new, opn, svn, sva, opg, svg, udo, rdo, cpy, pst, cut, sla, sle, sln, hlp') <- buildMaybeMenubar
  (treePanel, treeview, renderer, btnNew, btnRemove) <- buildTreePanel
  (window,canvas,hPaneMain) <- buildMainWindow maybeMenubar frameT treePanel

  -- criar os modelos para as comboboxes do buildHostMenu
  comboBoxSetModelText comboBoxNodeType
  sequence $ map (comboBoxAppendText comboBoxNodeType . T.pack) ["None", "A", "B", "C"]
  comboBoxSetActive comboBoxNodeType 0

  comboBoxSetModelText comboBoxEdgeType
  sequence $ map (comboBoxAppendText comboBoxEdgeType . T.pack) ["None", "AA", "AB", "AC" "BB", "BC", "CC"]
  comboBoxSetActive comboBoxEdgeType 0



  -- criar uma estrutura para armazenar na treeStore
  let typeGraphTree = Node (Store MenuGraph "TypeGraphs") [Node (Store (TypeGraph "this is a typeGraph.") "TypeGraph") []]
      graphTree = Node (Store MenuGraph "Graphs") [Node (Store (HostGraph "this is a graph.") "Graph") []]
  store <- treeStoreNew [Node (Store MenuGraph "Project") [typeGraphTree, graphTree]]
  projectCol <- treeViewGetColumn treeview 0
  case projectCol of
    Nothing -> return ()
    Just col -> do
      treeViewSetModel treeview (Just store)
      cellLayoutSetAttributes col renderer store $ \(Store t n) -> [cellText:=n]
      treeViewExpandAll treeview

  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  treeview `on` cursorChanged $ do
    selection <- treeViewGetSelection treeview
    maybeIt <- treeSelectionGetSelected selection
    case maybeIt of
      Nothing -> return ()
      Just it -> do
        path <- treeModelGetPath store it
        (Store t n) <- treeStoreGetValue store path
        changePropMenu t hPaneMain propMenuFrames
        widgetShowAll window

  widgetShowAll window
  mainGUI


changePropMenu :: GraphType -> HPaned -> (Frame,Frame) -> IO ()
changePropMenu MenuGraph _ _ = return ()

changePropMenu (HostGraph info) hpane (hFrame,tFrame) = do
  panedchild <- panedGetChild2 hpane
  case panedchild of
    Nothing -> panedPack2 hpane hFrame False True
    Just frame -> if toWidget hFrame == frame
      then return ()
      else do
        containerRemove hpane frame
        panedPack2 hpane hFrame False True

changePropMenu (TypeGraph info) hpane (hFrame,tFrame) = do
  panedchild <- panedGetChild2 hpane
  case panedchild of
    Nothing -> panedPack2 hpane tFrame False True
    Just frame -> if toWidget tFrame == frame
      then return ()
      else do
        containerRemove hpane frame
        panedPack2 hpane tFrame False True

changePropMenu _ _ _ = return ()
