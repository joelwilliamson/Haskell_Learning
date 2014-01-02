module PriorityQueue (
	PriorityQueue(Empty),
	makeNode,
	peek,
	enqueue,
	enqueueList,
	dequeue
	) where
	
import Data.Maybe

data PriorityQueue a = PQNode a (PriorityQueue a) (PriorityQueue a) Int Int
			| Empty
	deriving (Show,Eq)

makeNode :: (Ord a) => a -> (PriorityQueue a) -> (PriorityQueue a) -> (PriorityQueue a)
makeNode x left right = PQNode x left right (1 + (max (maxHeight left) (maxHeight right))) (1 + (min (minHeight left) (minHeight right)))

-- Return the largest element in the queue without changing it
peek :: (Ord a) => (PriorityQueue a) -> (Maybe a)
peek Empty = Nothing
peek (PQNode v x y _ _) = Just v

maxHeight :: (Num b) => (PriorityQueue a) -> b
maxHeight (PQNode _ _ _ maxHeight _) = fromIntegral maxHeight
maxHeight Empty = 0

minHeight :: (Num b) => (PriorityQueue a) -> b
minHeight (PQNode _ _ _ _ minHeight) = fromIntegral minHeight
minHeight Empty = 0

left :: (PriorityQueue a) -> (PriorityQueue a)
left Empty = Empty
left (PQNode _ l _ _ _) = l

right :: (PriorityQueue a) -> (PriorityQueue a)
right Empty = Empty
right (PQNode _ _ r _ _) = r

enqueue :: (Ord a) => (PriorityQueue a) -> a -> (PriorityQueue a)
enqueue Empty x = PQNode x Empty Empty 1 1
enqueue (PQNode top left right _ _) x = makeNode top' left' right'
	where 	top' = max top x
		leftFull = maxHeight left == minHeight left
		allFull = minHeight right == maxHeight left
		left' = if allFull || not leftFull
			then enqueue left $ min top x
			else left
		right' = if leftFull && not allFull
			then enqueue right $ min top x
			else right

enqueueList :: (Ord a) => (PriorityQueue a) -> [a] -> (PriorityQueue a)
enqueueList = foldl enqueue

removeRightmost :: (Ord a) => (PriorityQueue a) -> (Maybe a, (PriorityQueue a))
removeRightmost (PQNode top Empty Empty 1 1) = (Just top, Empty)
removeRightmost (PQNode top left right mx mn) = (rm, makeNode top left' right')
	where 	targetLeft = (maxHeight left > minHeight left) || (maxHeight left > maxHeight right)
		(leftRm, left') =	if targetLeft
					then removeRightmost left
					else (Nothing, left)
		(rightRm, right') =	if targetLeft
					then (Nothing, right)
					else removeRightmost right
		rm = max leftRm rightRm

dequeue :: (Ord a) => (PriorityQueue a) -> (PriorityQueue a)
dequeue Empty = Empty
dequeue (PQNode _ _ _ 1 _) = Empty
dequeue (PQNode top left Empty _ _) = left
dequeue (PQNode top left right 2 2) = makeNode top' left' Empty
	where 	top' = (max (fromJust $ peek left) (fromJust $ peek right))
		left' = makeNode (min (fromJust $ peek left) (fromJust $ peek right)) Empty Empty
dequeue pq = makeNode top' left' right'
	where	((Just rm),pq') = removeRightmost pq
		leftMax = peek $ left pq'
		rightMax = peek $ right pq'
		top' = fromJust $ max leftMax rightMax
		targetLeft = leftMax > rightMax
		left' = if targetLeft
			then enqueue (dequeue $ left pq') rm
			else left pq'
		right'= if targetLeft
			then right pq'
			else enqueue (dequeue $ right pq') rm
