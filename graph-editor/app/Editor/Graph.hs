module Editor.Graph
( Graph (..)
, string2graph
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
, graphUnion
, Node (..)
, nodeGetID
, nodeGetInfo
, nullNode
, Edge (..)
, edgeGetID
, edgeGetInfo
)where

import Data.List
import Editor.Helper

-- Estruturas de conteudo ------------------------------------------------------

-- | Info - conteúdo de um nodo
type Info = String


-- Estruturas do Grafo ---------------------------------------------------------------
-- Nodo
-- Node id informação
data Node = Node Int Info deriving (Eq,Show, Read)

-- Comparação de nodos
-- O que importa é o ID
instance Ord Node where
  (Node nID1 _) <= (Node nID2 _) = nID1 <= nID2

-- Getters
nodeGetID :: Node -> Int
nodeGetID (Node nID _) = nID

nodeGetInfo :: Node -> Info
nodeGetInfo (Node _ info) = info

nullNode :: Node
nullNode = Node 0 ""

-- Aresta
-- Edge id informação
data Edge = Edge Int Info deriving (Eq,Show, Read)

-- Comparação de Arestas
-- O que importa é o ID
instance Ord Edge where
  (Edge eID1 _) <= (Edge eID2 _) = eID1 <= eID2

-- Getters
edgeGetID :: Edge -> Int
edgeGetID (Edge eID _) = eID

edgeGetInfo :: Edge -> Info
edgeGetInfo (Edge _ info) = info

-- Grafo
-- Graph nome edges_src edges_dest edges nodes
type Name = String
data Graph = Graph Name (Edge->Int) (Edge->Int) [Edge] [Node]

instance Eq Graph where
  g1 == g2 = (ns1 == ns2) && (es1 == es2) && (conns1 == conns2)
    where
      ns1 = graphGetNodes g1
      ns2 = graphGetNodes g2
      es1 = graphGetEdges g1
      es2 = graphGetEdges g2
      conns1 = conns2tuples es1 (graphGetSrcFunc g1) (graphGetDstFunc g1)
      conns2 = conns2tuples es2 (graphGetSrcFunc g2) (graphGetDstFunc g2)


-- printar grafo
instance Show Graph where
  show = graph2string

-- funções auxiliares para função 'show'
conns2tuples :: [Edge] -> (Edge->Int) -> (Edge->Int) -> [(Int,Int,Int)]
conns2tuples es src dst = map (\e -> (edgeGetID e, src e, dst e)) es

graph2string :: Graph -> String
graph2string (Graph id src dst e n) = "Graph " ++ (show id) ++ " \n" ++ (show n) ++ "\n" ++ (show e) ++ "\n"  ++ (show $ conns2tuples e src dst)

-- função auxiliar para função string2graph
tuples2conns :: [(Int,Int,Int)] -> ((Edge->Int), (Edge->Int))
tuples2conns xs = foldl (\ (f1,f2) (e,n1,n2) -> (\a -> if edgeGetID a == e then n1 else f1 a, \a -> if edgeGetID a == e then n2 else f2 a) ) (\a -> 0, \a -> 0) xs

-- converte uma string para um grafo, semelhante a função read, e retorna o restante das linhas
string2graph :: String -> (Graph, String)
string2graph s = (Graph name src dst edges nodes, rest)
  where slines = lines s
        name = read (unwords . tail . words $ (slines!!0)) :: String
        nodes = read (slines!!1) :: [Node]
        edges = read (slines!!2) :: [Edge]
        (src,dst) = tuples2conns (read (slines!!3) :: [(Int,Int,Int)])
        rest = unlines $ (tails slines)!!4

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
  if (nodeGetID n `notElem` (map nodeGetID ns))
    then Graph iD src dst e (n:ns)
    else Graph iD src dst e ns

-- remove um nodo do grafo
removeNode :: Graph -> Node -> Graph
removeNode g n =
  Graph iD src dst es ns'
  -- removeConnectors defined bellow ↓↓↓↓↓
  where (Graph iD src dst es ns) = removeConnectors g n
        ns' = filter (\node -> nodeGetID node /= nodeGetID n) ns

-- muda um nodo do grafo
changeNode :: Graph -> Node -> Graph
changeNode (Graph iD src dst es ns) n =
  let newNodes = map (\node -> if nodeGetID node == nodeGetID n then n else node) ns
  in Graph iD src dst es newNodes

-- insere uma aresta no grafo
insertEdge :: Graph -> Node -> Node -> Graph
insertEdge (Graph iD src dst es ns) n1 n2 = if haveNodes then Graph iD src' dst' (ne:es) ns else Graph iD src dst es ns
  where haveNodes = all (`elem` ns) [n1,n2]
        neID = if length es > 0 then (maximum (map edgeGetID es)) + 1 else 1
        ne = Edge neID ""
        (nID1,nID2) = applyPair nodeGetID (n1,n2)
        src' = \e -> if edgeGetID e == neID then nID1 else src e
        dst' = \e -> if edgeGetID e == neID then nID2 else dst e

-- remove uma aresta do grafo
removeEdge :: Graph -> Edge -> Graph
removeEdge (Graph iD src dst es ns) e =
  if (edgeGetID e `elem` (map edgeGetID es))
  then let es' = filter (\edge -> edgeGetID edge /= edgeGetID e) es
           src' = \a -> if edgeGetID a == edgeGetID e then 0 else src a
           dst' = \a -> if edgeGetID a == edgeGetID e then 0 else dst a
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


-- juntar dois grafos
graphUnion :: Graph -> Graph -> Graph
graphUnion g1 g2 = g3
  where
    maxNid = let ns = graphGetNodes g1 in if null ns then 0 else nodeGetID $ maximum ns
    nodesG2 = graphGetNodes g2
    newNids = map (+maxNid) (let l = length nodesG2 in [l,(l-1)..1])
    newNodesMap = zipWith (\n nid -> (nodeGetID n, Node nid (nodeGetInfo n)) ) nodesG2 newNids
    connsG2 = map (\e -> (graphGetSrcFunc g2 e, graphGetDstFunc g2 e)) (sort $ graphGetEdges g2)
    connsNew = map (\conn -> applyPair (\nid -> lookup nid newNodesMap) conn) connsG2
    g1' = foldl insertNode g1 (map snd newNodesMap)
    g3 = foldl (\g conn -> case conn of
                            (Just a, Just b) -> insertEdge g a b
                            _ -> g)
        g1' connsNew