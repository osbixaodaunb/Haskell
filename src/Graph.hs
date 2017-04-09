infinity = 1e1000

data Node = Node {
     value :: Int
     }
            deriving(Show, Eq, Ord)
data Vertex = Vertex {
     adjacency :: (Node, Node),
     weight :: Int
     
     }
            deriving(Show)
data Graph = Graph [Vertex]
            deriving(Show)

createNodes 0 = []
createNodes num = (Node num) : (createNodes (num - 1))

nodes = createNodes 5

vertexes = [Vertex (nodes !! 0, nodes !! 1) 1, Vertex ( nodes !! 1, nodes !! 0) 1,
		 	Vertex (nodes !! 0, nodes !! 2) 1, Vertex (nodes !! 2, nodes !! 0) 1,
			Vertex (nodes !! 1, nodes !! 2) 13, Vertex ( nodes !! 2, nodes !! 1) 13,
			Vertex (nodes !! 1, nodes !! 4) 2, Vertex ( nodes !! 4, nodes !! 1) 2,
			Vertex (nodes !! 4, nodes !! 3) 8, Vertex ( nodes !! 3, nodes !! 4) 8,
			Vertex (nodes !! 2, nodes !! 3) 3, Vertex (nodes !! 3, nodes !! 2) 3]

g = Graph vertexes
