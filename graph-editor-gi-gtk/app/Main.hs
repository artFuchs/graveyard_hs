{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import Editor.UIBuilders
import qualified GI.Gtk as Gtk
import Data.GI.Base

main = do
  Gtk.init Nothing
  treePanel <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
  inspectorFrame <- buildTypeMenu
  (window,canvas,_) <- buildMainWindow (Nothing :: Maybe Gtk.Widget) inspectorFrame treePanel
  Gtk.widgetShowAll window
  Gtk.main
  return ()
