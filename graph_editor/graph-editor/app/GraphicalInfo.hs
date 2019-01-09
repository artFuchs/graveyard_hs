module GraphicalInfo
( GraphicalInfo (..)
, newGraphicalInfo
, giSetPosition
, giSetColor
, giSetLineColor
, giSetDims
, giSetCentered
)where

-- estrutura de dados para desenhar um nodo
data GraphicalInfo =  GraphicalInfo { position :: (Double, Double)
                                        , color :: (Double,Double,Double)
                                        , lineColor :: (Double,Double,Double)
                                        , dims :: (Double,Double)
                                        , centered :: Bool
                                        } deriving (Show)
-- GraphicalInfo {color, position}
-- position : posição na tela
-- color : para nodos - cor de preenchimento
-- lineColor : cor da linha
-- dims : dimensões do nodo / caixa de texto da label da aresta
-- centered : para arestas - se a aresta deve se manter centralizada entre dois nodos ou não

-- contrutor padrão do GraphicalInfo
newGraphicalInfo :: GraphicalInfo
newGraphicalInfo = GraphicalInfo  { position = (0,0)
                                  , color = (1,1,1)
                                  , lineColor = (0,0,0)
                                  , dims = (20,20)
                                  , centered = True
                                  }

-- métodos para "modificar" um graficalInfo
giSetPosition :: (Double, Double) -> GraphicalInfo -> GraphicalInfo
giSetPosition pos gi = GraphicalInfo  { position = pos
                                      , color = color gi
                                      , lineColor = lineColor gi
                                      , dims = dims gi
                                      , centered = centered gi
                                      }

giSetColor :: (Double, Double, Double) -> GraphicalInfo -> GraphicalInfo
giSetColor col gi = GraphicalInfo   { color = col
                                    , position = position gi
                                    , lineColor = lineColor gi
                                    , dims = dims gi
                                    , centered = centered gi
                                    }

giSetLineColor :: (Double, Double, Double) -> GraphicalInfo -> GraphicalInfo
giSetLineColor col gi = GraphicalInfo  { lineColor = col
                                    , position = position gi
                                    , color = color gi
                                    , dims = dims gi
                                    , centered = centered gi
                                    }

giSetDims :: (Double,Double) -> GraphicalInfo -> GraphicalInfo
giSetDims d gi = GraphicalInfo  { dims = d
                                , position = position gi
                                , color = color gi
                                , lineColor = lineColor gi
                                , centered = centered gi
                                }

giSetCentered :: Bool -> GraphicalInfo -> GraphicalInfo
giSetCentered c gi = GraphicalInfo { centered = c
                                   , position = position gi
                                   , color = color gi
                                   , lineColor = lineColor gi
                                   , dims = dims gi
                                   }
