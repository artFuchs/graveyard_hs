import Data.List

data GraphicalInfo = Color String | Position Float Float deriving (Show)

-- Graph Structs ---------------------------------------------------------------
data Info = Info String [GraphicalInfo] deriving (Show)
-- Node id content graphical_information
data Node = Node Int Info deriving (Show)

-- Node Comparation
-- Whats matter is the ID
instance Eq Node where
  (Node nid1 _) == (Node nid2 _ ) = nid1 == nid2

instance Ord Node where
  (Node nid1 _) <= (Node nid2 _) = nid1 <= nid2

-- Graph id edges_src edges_dest edges nodes
data Graph = Graph String (Int->Int) (Int->Int) [Int] [Node]

-- Show Graph
instance Show Graph where
  show = graphString

-- Auxiliar functions to Show Graph
edgeStr :: Int -> (Int->Int) -> (Int->Int) -> String
edgeStr e src dst = show e ++ ": " ++ ( show (src e) ) ++ " -> " ++ ( show (dst e) )

edgesStr :: [Int] -> (Int->Int) -> (Int->Int) -> String
edgesStr es src dst = foldl (\acc e -> acc ++ (edgeStr e src dst) ++ ", ") "" es

graphString :: Graph -> String
graphString (Graph id src dst e n) = "Graph " ++ (show id) ++ " : \n Nodes: " ++ (show n) ++ ";\n Edges: [" ++ (edgesStr e src dst) ++ "]"


-- base constructor for graph
emptyGraph :: String -> Graph
emptyGraph iD = Graph iD (\i -> 0) (\i -> 0) [] []




-- operations for graphs -------------------------------------------------------

-- insert a node
insertNode :: Graph -> Node -> Graph
insertNode (Graph iD src dst e ns) n =
  if (n `notElem` ns)
  then Graph iD src dst e (n:ns)
  else Graph iD src dst e ns

-- remove a node
removeNode :: Graph -> Node -> Graph
removeNode g n =
  Graph iD src dst es ns'
  -- removeConnections defined bellow ↓↓↓↓↓
  where (Graph iD src dst es ns) = removeConnections g n
        ns' = filter (\node -> node /= n) ns

-- insert an edge
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

-- remove an edge
removeEdge :: Graph -> Int -> Graph
removeEdge (Graph iD src dst es ns) e =
  if (e `elem` es)
  then let es' = filter (/=e) es
           src' = \a -> if a == e then 0 else src a
           dst' = \a -> if a == e then 0 else dst a
        in Graph iD src' dst' es' ns
  else Graph iD src dst es ns


-- given a node, get the edges connected to it
getConnections :: Graph -> Node -> [Int]
getConnections (Graph iD src dst es ns) (Node nid _) =
  filter (\e -> (nid == src e) || (nid == dst e)) es

-- given a node, remove all the edges connected to it
removeConnections :: Graph -> Node -> Graph
removeConnections g n =
  let es = getConnections g n
  in foldl (\accG e -> removeEdge accG e) g es

-- given a Node, update it's information





nodes1 :: [Node]
nodes1 =  [(Node 1 $ Info "hello" [])
          , (Node 2 $ Info "my" [])
          , (Node 3 $ Info "name" [])
          , (Node 4 $ Info "is" [])
          , (Node 5 $ Info "Mr. Fear" [])
          ]

graph1 :: Graph
graph1 = Graph "1" src1 dst1 [1,2,3,4,5] nodes1
-- ↓↓↓↓↓ src / dst functions to test the graph ↓↓↓↓↓ ---------------------------

-- 1 -1-> 2
-- 1 -2-> 3
-- 2 -3-> 4
-- 3 -4-> 4
-- 4 -5-> 5
src1 :: Int -> Int
src1 edge
    | edge == 1 = 1
    | edge == 2 = 1
    | edge == 3 = 2
    | edge == 4 = 3
    | edge == 5 = 4
    | otherwise = 0

dst1 :: Int -> Int
dst1 edge
    | edge == 1 = 2
    | edge == 2 = 3
    | edge == 3 = 4
    | edge == 4 = 4
    | edge == 5 = 5
    | otherwise = 0

-- 1 -1-> 3
-- 3 -2-> 5
-- 5 -3-> 2
-- 2 -4-> 4
-- 4 -5-> 1
src2 :: Int -> Int
src2 1 = 1
src2 2 = 3
src2 3 = 5
src2 4 = 2
src2 5 = 4
src2 _ = 0

dst2 :: Int -> Int
dst2 1 = 3
dst2 2 = 5
dst2 3 = 2
dst2 4 = 4
dst2 5 = 1
dst2 _ = 0
