import Debug.Trace
import Data.List

infinity = 1e1000

type Node = Integer
data Vertex = Vertex {
     adjacency :: (Node, Node),
     weight :: Float
     }
     deriving(Show)

type Graph = [Vertex]

createNodes 0 = []
createNodes num = (num) : (createNodes (num - 1))

nodes = reverse(createNodes 5)

vertexes = [Vertex (nodes !! 0, nodes !! 1) 1, Vertex ( nodes !! 1, nodes !! 0) 1,
		 	Vertex (nodes !! 0, nodes !! 2) 1, Vertex (nodes !! 2, nodes !! 0) 1,
			Vertex (nodes !! 1, nodes !! 2) 13, Vertex ( nodes !! 2, nodes !! 1) 13,
			Vertex (nodes !! 1, nodes !! 4) 2, Vertex ( nodes !! 4, nodes !! 1) 2,
			Vertex (nodes !! 4, nodes !! 3) 8, Vertex ( nodes !! 3, nodes !! 4) 8,
			Vertex (nodes !! 2, nodes !! 3) 3, Vertex (nodes !! 3, nodes !! 2) 3]

foreach [] _ = []			
foreach (x:xs) f = (f x) : (foreach xs f)

graph = vertexes
adjacencyGraph = map adjacency (graph)

listOfNodes = [1..5]

neighbors g n = map snd (filter (\(nd, _) -> nd == n) $ (map adjacency $ g))

edgesFor g n = snd . head . filter (\(nd, _) -> nd == n) $ g


weightInNode :: [Vertex] -> (Node,Node) -> Float
weightInNode g n = if null (filter (\v -> n == adjacency(v) ) $ g)
				   then 1.0/0.0
				   else weight . head . filter (\v -> n == adjacency(v) ) $ g

initD start = 
	let initDist es = 
		if es == start
		    then 0
		   	else if es `elem` (neighbors graph start)
		   		then weightInNode graph (start, es)
		   		else 1.0/0.0
	in map (\n -> (Vertex (start, n) (initDist(n)))) listOfNodes


dijkstra g start = 
	let 
		dnodes = initD start
		unchecked = listOfNodes
	in dijkstra' g dnodes unchecked

dijkstra' g dnodes [] = dnodes
dijkstra' g dnodes unchecked = 
	let 
		dunchecked = filter(\v -> (snd (adjacency v)) `elem` unchecked) dnodes
		current = head . sortBy (\(Vertex _ w1) (Vertex _ w2) -> compare w1 w2) $ dunchecked
		c = snd (adjacency current)
		newUnchecked = delete c unchecked
		neighbor = intersect (neighbors g c) newUnchecked
		newDNodes = map (\dn -> update dn current neighbor (neighbors g c)) dnodes
	in  if null dunchecked 
		then dnodes 
		else dijkstra' g newDNodes newUnchecked

update dn@(Vertex no@(k, n) nd) (Vertex (c, f) cd) cnodes edges = 
	let wt = weightInNode graph (n, f)
	in  if n `notElem` cnodes 
		then dn
		else if cd + wt < nd
			 then Vertex (f, n) (cd+wt)
			 else dn

dnodeForNode dnodes n = (head . filter (\p -> (snd (adjacency p)) == n) $ dnodes)

pathToNode dnodes dest = 
	let dn@(Vertex (n, p) d) = dnodeForNode dnodes dest
	in if n == p
		then [n]
		else [p] ++ pathToNode dnodes n