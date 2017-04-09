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

