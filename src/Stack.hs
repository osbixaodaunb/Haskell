import System.IO

data Stack value = Stack [value]
    deriving(Show)

emptyStack :: Stack value
emptyStack = Stack []

pushValue :: x -> Stack x -> Stack x
pushValue h (Stack ht) = Stack (h:ht)

popStack :: Stack x -> Stack x
popStack (Stack []) = Stack []
popStack (Stack (h:t)) = Stack t