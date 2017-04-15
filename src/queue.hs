-- data structure for a queue
data Queue elem = Queue [elem][elem]
				   deriving (Eq,Show)


-- function to push an element to the queue
push :: elem -> Queue elem -> Queue elem
push item (Queue inp out)  = Queue (item:inp) out

--function to pop an element of the queue
pop :: Queue elem -> (elem, Queue elem)
pop (Queue inp []) = pop $ Queue [] (reverse inp)
pop (Queue inp out) = (head out, Queue inp(tail out))



