infinity = 1e1000

type Node = Int
data Vertex = Vertex {
     adjacency :: (Node, Node),
     weight :: Int    
     } deriving(Show)
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

g = map adjacency (vertexes)

neighbors g n = filter (\(nd, _) -> nd == n) $ g

weightInNode v n = weight . head . filter (\e -> n == fst(adjacency e)) $ v

-- connectedNodes v = map fst(adjacency v) 

dnodeForNode dnodes n = head . filter (\(x, _) -> x == n) $ dnodes
