import Debug.Trace
--data definition of a tree

data Tree node = Null
    | Branch node (Tree node) (Tree node)
    deriving (Eq,Ord, Show)

--function to check if one element is in the tree

isElemIn :: (Ord node) => node -> Tree node -> Bool
isElemIn node Null = False
isElemIn node (Branch value left right)
    | node == value = True
    | node < value = isElemIn node left
    | otherwise =  isElemIn node right

--function to count the number of elements in a tree
numberElem :: Tree node -> Int
numberElem Null = 0
numberElem (Branch _ left right) = 1 + (numberElem left) + (numberElem right)

--function to print a tree as a list

printAscendent :: Tree node -> [node]
printAscendent Null = []
printAscendent (Branch value left right) = (printAscendent left) ++[value] ++ (printAscendent right)

printDescendent :: Tree node ->[node]
printDescendent Null = []
printDescendent (Branch value left right) = (printDescendent right) ++ [value] ++ (printDescendent left)

--function that inserts a node
insertNode :: (Eq node, Ord node) => Tree node -> node -> Tree node
insertNode Null node = Branch node Null Null
insertNode (Branch node left right) newNode
	| newNode == node = Null
	| newNode < node = (Branch node (insertNode left newNode) right)
	| otherwise = (Branch node left (insertNode right newNode))

minimumNode :: Tree node -> node
minimumNode (Branch node Null _)  = node
minimumNode (Branch _ left _) = minimumNode left

deleteNode :: (Eq node, Ord node) => Tree node -> node -> Tree node
deleteNode Null key = Null
deleteNode (Branch key left Null) _ = left
deleteNode (Branch key Null right) _  = right
deleteNode (Branch key' left right) key | key < key' = (Branch key' (deleteNode left key) right)
deleteNode (Branch key' left right) key | key > key' = (Branch key' left (deleteNode right key))  
deleteNode (Branch _ left right) _ = (Branch removeNode' left right')
	where removeNode' = minimumNode right;
		  right' = deleteNode (right) removeNode'








