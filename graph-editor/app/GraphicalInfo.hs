module GraphicalInfo
( GraphicalInfo (..)
, NodeShape (..)
, NodeGI (..)
, EdgeStyle (..)
, EdgeGI (..)
, newNodeGI
, newEdgeGI
, nodeGiSetPosition
, nodeGiSetColor
, nodeGiSetLineColor
, nodeGiSetDims
, nodeGiSetShape
, edgeGiSetPosition
, edgeGiSetColor
, edgeGiSetCentered
, edgeGiSetStyle
)where

import qualified Data.Map as M

type GraphicalInfo = (M.Map Int NodeGI, M.Map Int EdgeGI)

-- estrutura auxiliar - define a forma de um nodo
data NodeShape = NCircle | NRect | NQuad deriving (Eq ,Show, Read)

-- estrutura de dados para desenhar um nodo
-- position : posição do nodo
-- color : cor de preenchimento
-- lineColor : cor da linha e texto
-- dims : dimensões do nodo
-- shape : forma do nodo
data NodeGI = NodeGI { position :: (Double, Double)
                     , fillColor :: (Double,Double,Double)
                     , lineColor :: (Double,Double,Double)
                     , dims :: (Double,Double)
                     , shape :: NodeShape
                     } deriving (Eq, Show, Read)

data EdgeStyle = ENormal | EPointed deriving (Eq ,Show, Read)

-- estrutura de dados para desenhar uma aresta
-- cPosition : posição do ponto de controle
-- color : cor da linha e texto
-- centered : se a aresta deve se manter centralizada entre dois nodos ou não
data EdgeGI = EdgeGI { cPosition :: (Double,Double)
                     , color :: (Double,Double,Double)
                     , centered :: Bool
                     , style :: EdgeStyle
                     } deriving (Eq, Show, Read)
-- contrutores
newNodeGI :: NodeGI
newNodeGI = NodeGI  { position = (0,0)
                    , fillColor = (1,1,1)
                    , lineColor = (0,0,0)
                    , dims = (20,20)
                    , shape = NCircle
                    }

newEdgeGI :: EdgeGI
newEdgeGI = EdgeGI { cPosition = (0,0)
                   , color = (0,0,0)
                   , centered = True
                   , style = ENormal
                   }

-- métodos para "modificar" um NodeGI
nodeGiSetPosition :: (Double, Double) -> NodeGI -> NodeGI
nodeGiSetPosition pos gi = NodeGI { position = pos
                              , fillColor = fillColor gi
                              , lineColor = lineColor gi
                              , dims = dims gi
                              , shape = shape gi
                              }

nodeGiSetColor :: (Double, Double, Double) -> NodeGI -> NodeGI
nodeGiSetColor col gi = NodeGI { position = position gi
                               , fillColor = col
                               , lineColor = lineColor gi
                               , dims = dims gi
                               , shape = shape gi
                               }

nodeGiSetLineColor :: (Double, Double, Double) -> NodeGI -> NodeGI
nodeGiSetLineColor col gi = NodeGI  { position = position gi
                                    , lineColor = col
                                    , fillColor = fillColor gi
                                    , dims = dims gi
                                    , shape = shape gi
                                    }

nodeGiSetDims :: (Double,Double) -> NodeGI -> NodeGI
nodeGiSetDims d gi = NodeGI { dims = d
                        , position = position gi
                        , fillColor = fillColor gi
                        , lineColor = lineColor gi
                        , shape = shape gi
                        }

nodeGiSetShape :: NodeShape -> NodeGI -> NodeGI
nodeGiSetShape s gi = NodeGI { position = position gi
                             , fillColor = fillColor gi
                             , lineColor = lineColor gi
                             , dims = dims gi
                             , shape = s
                             }

-- métodos para "modificar" um EdgeGI
edgeGiSetPosition :: (Double,Double) -> EdgeGI -> EdgeGI
edgeGiSetPosition pos gi = EdgeGI { cPosition = pos
                                  , color = color gi
                                  , centered = centered gi
                                  , style = style gi
                                  }

edgeGiSetColor :: (Double,Double,Double) -> EdgeGI -> EdgeGI
edgeGiSetColor col gi = EdgeGI { cPosition = cPosition gi
                                  , color = col
                                  , centered = centered gi
                                  , style = style gi
                                  }

edgeGiSetCentered :: Bool -> EdgeGI -> EdgeGI
edgeGiSetCentered c gi = EdgeGI { cPosition = cPosition gi
                                , color = color gi
                                , centered = c
                                , style = style gi
                                }

edgeGiSetStyle :: EdgeStyle -> EdgeGI -> EdgeGI
edgeGiSetStyle s gi = EdgeGI { cPosition = cPosition gi
                             , color = color gi
                             , centered = centered gi
                             , style = s
                             }
