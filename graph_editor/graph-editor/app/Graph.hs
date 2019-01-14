module Graph
( Graph (..)
, emptyGraph
, graphGetID
, graphGetNodes
, graphGetEdges
, graphGetDstFunc
, graphGetSrcFunc
, insertNode
, changeNode
, removeNode
, insertEdge
, removeEdge
, changeEdge
, getConnectors
, getNodeByID
, getDstNode
, getSrcNode
, Node (..)
, nodeGetID
, nodeGetInfo
, nodeGetGI
, Edge (..)
, edgeGetID
, edgeGetInfo
, edgeGetGI
)where

import Data.List
import GraphicalInfo
import Helper

-- Estruturas de conteudo ------------------------------------------------------

-- | Info - conteúdo de um nodo
type Info = String


-- Estruturas do Grafo ---------------------------------------------------------------
-- Nodo
-- Node id informação
data Node = Node Int Info NodeGI deriving (Show)

-- Comparação de nodos
-- O que importa é o ID
instance Eq Node where
  (Node nID1 _ _) == (Node nID2 _ _) = nID1 == nID2

instance Ord Node where
  (Node nID1 _ _) <= (Node nID2 _ _) = nID1 <= nID2

-- Getters
nodeGetID :: Node -> Int
nodeGetID (Node nID _ _) = nID

nodeGetInfo :: Node -> Info
nodeGetInfo (Node _ info _) = info

nodeGetGI :: Node -> NodeGI
nodeGetGI (Node _ _ gi) = gi

-- Aresta
-- Edge id informação
data Edge = Edge Int Info EdgeGI

-- Comparação de Arestas
-- O que importa é o ID
instance Eq Edge where
  (Edge eID1 _ _) == (Edge eID2 _ _) = eID1 == eID2

instance Ord Edge where
  (Edge eID1 _ _) <= (Edge eID2 _ _) = eID1 <= eID2

-- Getters
edgeGetID :: Edge -> Int
edgeGetID (Edge eID _ _) = eID

edgeGetInfo :: Edge -> Info
edgeGetInfo (Edge _ info _) = info

edgeGetGI :: Edge -> EdgeGI
edgeGetGI (Edge _ _ gi) = gi

-- Grafo
-- Graph nome edges_src edges_dest edges nodes
type Name = String
data Graph = Graph Name (Edge->Int) (Edge->Int) [Edge] [Node]

-- printar grafo
instance Show Graph where
  show = graphString

-- funções auxiliares para função 'show'
edgeStr :: Edge -> (Edge->Int) -> (Edge->Int) -> String
edgeStr e src dst = show (edgeGetID e) ++ ": " ++ ( show (src e) ) ++ " -> " ++ ( show (dst e) )

edgesStr :: [Edge] -> (Edge->Int) -> (Edge->Int) -> String
edgesStr es src dst = foldl (\acc e -> acc ++ (edgeStr e src dst) ++ ", ") "" es

graphString :: Graph -> String
graphString (Graph id src dst e n) = "Graph " ++ (show id) ++ " : \n Nodes: " ++ (show n) ++ ";\n Edges: [" ++ (edgesStr e src dst) ++ "]"


-- construtor base para o grafo
emptyGraph :: String -> Graph
emptyGraph iD = Graph iD (\i -> 0) (\i -> 0) [] []

-- Getters
graphGetNodes :: Graph -> [Node]
graphGetNodes (Graph _ _ _ _ ns) = ns

graphGetEdges :: Graph -> [Edge]
graphGetEdges (Graph _ _ _ es _) = es

graphGetID :: Graph -> String
graphGetID (Graph iD _ _ _ _) = iD

graphGetSrcFunc :: Graph -> (Edge->Int)
graphGetSrcFunc (Graph _ src _ _ _) = src

graphGetDstFunc :: Graph -> (Edge->Int)
graphGetDstFunc (Graph _ _ dst _ _) = dst



-- OPERAÇÕES SOBRE GRAFOS ------------------------------------------------------

-- insere um nodo no grafo
insertNode :: Graph -> Node -> Graph
insertNode (Graph iD src dst e ns) n =
  if (n `notElem` ns)
    then Graph iD src dst e (n:ns)
    else Graph iD src dst e ns

-- remove um nodo do grafo
removeNode :: Graph -> Node -> Graph
removeNode g n =
  Graph iD src dst es ns'
  -- removeConnectors defined bellow ↓↓↓↓↓
  where (Graph iD src dst es ns) = removeConnectors g n
        ns' = filter (\node -> node /= n) ns

-- muda um nodo do grafo
changeNode :: Graph -> Node -> Graph
changeNode (Graph iD src dst es ns) n =
  let newNodes = map (\node -> if node == n then n else node) ns
  in Graph iD src dst es newNodes

-- insere uma aresta no grafo
insertEdge :: Graph -> Node -> Node -> Graph
insertEdge (Graph iD src dst es ns) n1 n2 = Graph iD src' dst' (ne:es) ns
  where neID = if length es > 0 then (maximum (map edgeGetID es)) + 1 else 1
        pos1 = position . nodeGetGI $ n1
        pos2 = position . nodeGetGI $ n2
        gi = if n1 == n2
         then edgeGiSetPosition (fst pos1 + 30, snd pos1 + 30) newEdgeGI
         else edgeGiSetPosition (midPoint pos1 pos2) newEdgeGI
        ne = Edge neID "" gi
        nID1 = nodeGetID n1
        nID2 = nodeGetID n2
        src' = \e -> if e == ne then nID1 else src e
        dst' = \e -> if e == ne then nID2 else dst e

-- remove uma aresta do grafo
removeEdge :: Graph -> Edge -> Graph
removeEdge (Graph iD src dst es ns) e =
  if (e `elem` es)
  then let es' = filter (/=e) es
           src' = \a -> if a == e then 0 else src a
           dst' = \a -> if a == e then 0 else dst a
        in Graph iD src' dst' es' ns
  else Graph iD src dst es ns

-- muda as informações de uma aresta no grafo
changeEdge :: Graph -> Edge -> Graph
changeEdge (Graph iD src dst es ns) e =
  let newEdges = map (\edge -> if edgeGetID edge == edgeGetID e then e else edge) es
  in Graph iD src dst newEdges ns


-- dado um nodo, pegar as arestas conectadas a ele
getConnectors :: Graph -> Node -> [Edge]
getConnectors (Graph iD src dst es ns) n =
  filter (\e -> (nodeGetID n == src e) || (nodeGetID n == dst e)) es

-- dado um nodo, remover as arestas conectadas a ele
removeConnectors :: Graph -> Node -> Graph
removeConnectors g n =
  let es = getConnectors g n
  in foldl (\accG e -> removeEdge accG e) g es

-- dada uma aresta, procurar o nó source
getSrcNode :: Graph -> Edge -> Maybe Node
getSrcNode g e = find (\n -> nodeGetID n == src e ) ns
  where src = graphGetSrcFunc g
        ns = graphGetNodes g

-- dada uma aresta, procurar o nó destino
getDstNode :: Graph -> Edge -> Maybe Node
getDstNode g e = find (\n -> nodeGetID n == dst e ) ns
  where dst = graphGetDstFunc g
        ns = graphGetNodes g

-- dado um id, procurar o nó correspondente
getNodeByID :: Graph -> Int -> Maybe Node
getNodeByID g nID = find (\n -> nodeGetID n == nID) ns
  where ns = graphGetNodes g
