module GraphicalInfo
( GraphicalInfo (..)
, newGraphicalInfo
, giSetPosition
, giSetColor
, giSetLineColor
)where

-- estrutura de dados para desenhar um nodo
data GraphicalInfo =  GraphicalInfo { position :: (Double, Double)
                                      ,color :: (Double,Double,Double)
                                      , lineColor :: (Double,Double,Double)
                                      } deriving (Show)
-- GraphicalInfo {color, position}
-- position : posição na tela
-- color : para nodos: cor de preenchimento
-- lineColor : cor da linha


-- contrutor padrão do GraphicalInfo
newGraphicalInfo :: GraphicalInfo
newGraphicalInfo = GraphicalInfo  { position = (0,0)
                                  , color = (1,1,1)
                                  , lineColor = (0,0,0)
                                  }

-- métodos para "modificar" um graficalInfo
giSetPosition :: GraphicalInfo -> (Double, Double) -> GraphicalInfo
giSetPosition gi pos = GraphicalInfo  { position = pos
                                      , color = color gi
                                      , lineColor = lineColor gi
                                      }

giSetColor :: GraphicalInfo -> (Double, Double, Double) -> GraphicalInfo
giSetColor gi col = GraphicalInfo  { color = col
                                    , position = position gi
                                    , lineColor = lineColor gi
                                    }

giSetLineColor :: GraphicalInfo -> (Double, Double, Double) -> GraphicalInfo
giSetLineColor gi col = GraphicalInfo  { lineColor = col
                                    , position = position gi
                                    , color = color gi
                                    }
