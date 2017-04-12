
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





