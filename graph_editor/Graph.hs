module Graph
( Graph (..)
, emptyGraph
, graphGetID
, graphGetNodes
, graphGetEdges
, graphGetDstFunc
, graphGetSrcFunc
, insertNode
, removeNode
, insertEdge
, removeEdge
, getConnectors
, getNodeByID
, getDstNode
, getSrcNode
, Node (..)
, nodeGetID
, nodeGetInfo
, Info (..)
, infoGetContent
, infoGetGraphicalInfo
)where

import Data.List
import GraphicalInfo

-- Estruturas de conteudo ------------------------------------------------------

-- | Info - conteúdo de um nodo
type Content = String
data Info = Info Content GraphicalInfo deriving (Show)

infoGetContent :: Info -> Content
infoGetContent (Info content _) = content

infoGetGraphicalInfo :: Info -> GraphicalInfo
infoGetGraphicalInfo (Info _ graphicalInfo ) = graphicalInfo


-- Estruturas do Grafo ---------------------------------------------------------------
-- Nodo
-- Node id conteúdo informação_gráfica
data Node = Node Int Info deriving (Show)

-- Node Comparation
-- Whats matter is the ID
instance Eq Node where
  (Node nid1 _) == (Node nid2 _ ) = nid1 == nid2

instance Ord Node where
  (Node nid1 _) <= (Node nid2 _) = nid1 <= nid2

nodeGetID :: Node -> Int
nodeGetID (Node iD _) = iD

nodeGetInfo :: Node -> Info
nodeGetInfo (Node _ info) = info


-- Grafo
-- Graph nome edges_src edges_dest edges nodes
type Name = String
data Graph = Graph Name (Int->Int) (Int->Int) [Int] [Node]

-- printar grafo
instance Show Graph where
  show = graphString

-- funções auxiliares para função 'show'
edgeStr :: Int -> (Int->Int) -> (Int->Int) -> String
edgeStr e src dst = show e ++ ": " ++ ( show (src e) ) ++ " -> " ++ ( show (dst e) )

edgesStr :: [Int] -> (Int->Int) -> (Int->Int) -> String
edgesStr es src dst = foldl (\acc e -> acc ++ (edgeStr e src dst) ++ ", ") "" es

graphString :: Graph -> String
graphString (Graph id src dst e n) = "Graph " ++ (show id) ++ " : \n Nodes: " ++ (show n) ++ ";\n Edges: [" ++ (edgesStr e src dst) ++ "]"


-- construtor base para o grafo
emptyGraph :: String -> Graph
emptyGraph iD = Graph iD (\i -> 0) (\i -> 0) [] []

-- Getters
graphGetNodes :: Graph -> [Node]
graphGetNodes (Graph _ _ _ _ ns) = ns

graphGetEdges :: Graph -> [Int]
graphGetEdges (Graph _ _ _ es _) = es

graphGetID :: Graph -> String
graphGetID (Graph iD _ _ _ _) = iD

graphGetSrcFunc :: Graph -> (Int->Int)
graphGetSrcFunc (Graph _ src _ _ _) = src

graphGetDstFunc :: Graph -> (Int->Int)
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

-- insere uma aresta no grafo
insertEdge :: Graph -> Node -> Node -> Graph
insertEdge (Graph iD src dst es ns) n1 n2 =
  if (n1 `elem` ns) && (n2 `elem` ns)
  then let le = (length es) + 1
           Node nid1 _ = n1
           Node nid2 _ = n2
           src' = \e -> if e == le then nid1 else src e
           dst' = \e -> if e == le then nid2 else dst e
       in Graph iD src' dst' (le:es) ns
  else Graph iD src dst es ns

-- remove uma aresta do grafo
removeEdge :: Graph -> Int -> Graph
removeEdge (Graph iD src dst es ns) e =
  if (e `elem` es)
  then let es' = filter (/=e) es
           src' = \a -> if a == e then 0 else src a
           dst' = \a -> if a == e then 0 else dst a
        in Graph iD src' dst' es' ns
  else Graph iD src dst es ns


-- dado um nodo, pegar as arestas conectadas a ele
getConnectors :: Graph -> Node -> [Int]
getConnectors (Graph iD src dst es ns) (Node nid _) =
  filter (\e -> (nid == src e) || (nid == dst e)) es

-- dado um nodo, remover as arestas conectadas a ele
removeConnectors :: Graph -> Node -> Graph
removeConnectors g n =
  let es = getConnectors g n
  in foldl (\accG e -> removeEdge accG e) g es

-- dada uma aresta, procurar o nó source
getSrcNode :: Graph -> Int -> Maybe Node
getSrcNode g e = find (\n -> nodeGetID n == src e ) ns
  where src = graphGetSrcFunc g
        ns = graphGetNodes g

-- dada uma aresta, procurar o nó destino
getDstNode :: Graph -> Int -> Maybe Node
getDstNode g e = find (\n -> nodeGetID n == dst e ) ns
  where dst = graphGetDstFunc g
        ns = graphGetNodes g

-- dado um id, procurar o nó correspondente
getNodeByID :: Graph -> Int -> Maybe Node
getNodeByID g nid = find (\n -> nodeGetID n == nid) ns
  where ns = graphGetNodes g
