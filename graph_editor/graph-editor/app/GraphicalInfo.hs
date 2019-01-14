module GraphicalInfo
( NodeGI (..)
, EdgeGI (..)
, newNodeGI
, newEdgeGI
, nodeGiSetPosition
, nodeGiSetColor
, nodeGiSetLineColor
, nodeGiSetDims
, edgeGiSetPosition
, edgeGiSetColor
, edgeGiSetCentered
)where

-- estrutura de dados para desenhar um nodo
-- position : posição do nodo
-- color : cor de preenchimento
-- lineColor : cor da linha e texto
-- dims : dimensões do nodo
data NodeGI = NodeGI { position :: (Double, Double)
                     , fillColor :: (Double,Double,Double)
                     , lineColor :: (Double,Double,Double)
                     , dims :: (Double,Double)
                     } deriving (Show)

-- estrutura de dados para desenhar uma aresta
-- cPosition : posição do ponto de controle
-- color : cor da linha e texto
-- centered : se a aresta deve se manter centralizada entre dois nodos ou não
data EdgeGI = EdgeGI { cPosition :: (Double,Double)
                     , color :: (Double,Double,Double)
                     , centered :: Bool
                     }
-- contrutores
newNodeGI :: NodeGI
newNodeGI = NodeGI  { position = (0,0)
                    , fillColor = (1,1,1)
                    , lineColor = (0,0,0)
                    , dims = (20,20)
                    }

newEdgeGI :: EdgeGI
newEdgeGI = EdgeGI { cPosition = (0,0)
                   , color = (0,0,0)
                   , centered = True
                   }

-- métodos para "modificar" um NodeGI
nodeGiSetPosition :: (Double, Double) -> NodeGI -> NodeGI
nodeGiSetPosition pos gi = NodeGI { position = pos
                              , fillColor = fillColor gi
                              , lineColor = lineColor gi
                              , dims = dims gi
                              }

nodeGiSetColor :: (Double, Double, Double) -> NodeGI -> NodeGI
nodeGiSetColor col gi = NodeGI { fillColor = col
                           , position = position gi
                           , lineColor = lineColor gi
                           , dims = dims gi
                           }

nodeGiSetLineColor :: (Double, Double, Double) -> NodeGI -> NodeGI
nodeGiSetLineColor col gi = NodeGI  { lineColor = col
                                , position = position gi
                                , fillColor = fillColor gi
                                , dims = dims gi
                                }

nodeGiSetDims :: (Double,Double) -> NodeGI -> NodeGI
nodeGiSetDims d gi = NodeGI { dims = d
                        , position = position gi
                        , fillColor = fillColor gi
                        , lineColor = lineColor gi
                        }


-- métodos para "modificar" um EdgeGI
edgeGiSetPosition :: (Double,Double) -> EdgeGI -> EdgeGI
edgeGiSetPosition pos gi = EdgeGI { cPosition = pos
                                  , color = color gi
                                  , centered = centered gi
                                  }

edgeGiSetColor :: (Double,Double,Double) -> EdgeGI -> EdgeGI
edgeGiSetColor col gi = EdgeGI { cPosition = cPosition gi
                                  , color = col
                                  , centered = centered gi
                                  }

edgeGiSetCentered :: Bool -> EdgeGI -> EdgeGI
edgeGiSetCentered c gi = EdgeGI { cPosition = cPosition gi
                                , color = color gi
                                , centered = c
                                }
