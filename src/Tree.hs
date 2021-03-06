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

-- function that return the left-most node
minimumNode :: Tree node -> node
minimumNode (Branch node Null _)  = node
minimumNode (Branch _ left _) = minimumNode left

-- function that deletes the root node on a sub-tree
deleteRoot :: Tree node -> Tree node
deleteRoot (Branch node Null right) = right
deleteRoot (Branch node left Null) = left
deleteRoot (Branch node left right) = (Branch node' left right')
  where node' = minimumNode right
        right' = deleteRoot right

-- function that deletes the node' on a sub-tree
deleteAnyNode :: (Ord node) => Tree node -> node -> Tree node
deleteAnyNode Null _ = Null
deleteAnyNode (Branch node left right) node'
  | node' == node = deleteRoot (Branch node left right)
  | node' < node = (Branch node (deleteAnyNode left node') right)
  | otherwise = Branch node left (deleteAnyNode right node')

--function to check the heigh of a node
height :: Tree node -> Int
height Null  = 0
height (Branch _ left right) = 1 + max (height left) (height right)

--function to check if the tree is balenced
balance :: Tree node -> Bool
balance Null = True
balance (Branch _ left right) = (abs ( (height left) - (height right))) <= 1 && (balance  left) && (balance right)

--rotation to right
rotationR :: Tree node -> Tree node
rotationR Null = Null
rotationR (Branch value1 (Branch value2 value2left value2right) right) = (Branch value2 (value2left)(Branch value1 value2right right));

--rotation to left
rotationL :: Tree node -> Tree node
rotationL Null = Null
rotationL (Branch value1 value1left (Branch value2 value2left value2right)) = (Branch value2 (Branch value1 value1left value2left)(value2right) );

--function to return the balance factor of a node
balanceFactor :: Tree node -> Int
balanceFactor Null = 0
balanceFactor (Branch _ left right) = (height(right) - height(left))

getLeftNode (Branch _ left _) = left
getRightNode (Branch _ _ right) = right
getValueNode (Branch x _ _) = x

-- double rotation right left
rotationRL :: Tree node -> Tree node
rotationRL Null = Null
rotationRL (Branch value left right) =rotationL(Branch value (left) (rotationR(right)) )

-- double rotation left right
rotationLR :: Tree node -> Tree node
rotationLR Null = Null
rotationLR (Branch value left right) = rotationR(Branch value (rotationL(left))  (right))

--function to balance a tree
balanceTree :: Tree node -> Tree node
balanceTree Null = Null
balanceTree a = if balanceFactor(a) > 1 
	then if balanceFactor(getRightNode(a)) > 0
		then rotationL(a)
		else rotationRL(a)
	else if balanceFactor(a) < -1 
		then if balanceFactor(getLeftNode(a)) < 0 
			then rotationR(a)
			else rotationLR(a)
		else a
