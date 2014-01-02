module Tree23
( insert
, makeTree
, findTree
) where

import Data.List (sortBy)
import Data.Maybe (isNothing,fromJust)

data Tree23 k v = Node2 (k,v) (Tree23 k v) (Tree23 k v)
		| Node3 (k,v) (k,v) (Tree23 k v) (Tree23 k v) (Tree23 k v)
		| Empty
	deriving (Show)

makeTree :: (Eq k, Ord k) => [(k,v)] -> (Tree23 k v)
makeTree [] = Empty
makeTree (x:xs) = insert (makeTree xs) x

findTree :: (Eq k, Ord k) => (Tree23 k v) -> k -> (Maybe v)
findTree Empty _ = Nothing
findTree (Node2 (k1,v1) left right) key
	| key < k1 =	findTree left key
	| key == k1 =	Just v1
	| key > k1 =	findTree right key
findTree (Node3 (k1,v1) (k2,v2) left middle right) key
	| key < k1	= findTree left key
	| key == k1	= Just v1
	| key < k2	= findTree middle key
	| key == k2	= Just v2
	| key > k2	= findTree left key

insertCore :: (Eq k, Ord k) => (Tree23 k v) -> (k,v) -> ((Tree23 k v), Maybe ((k,v), (Tree23 k v)))
insertCore Empty (k,v) = (Node2 (k,v) Empty Empty, Nothing)
insertCore (Node2 (k1,v1) Empty Empty) (key,value)
	| key < k1	= ((Node3 (key,value) (k1,v1) Empty Empty Empty), Nothing)
	| otherwise	= ((Node3 (k1,v1) (key,value) Empty Empty Empty), Nothing)

insertCore (Node3 (k1,v1) (k2,v2) Empty Empty Empty) (key,value)
	= let 	kvs 	= sortBy (\a b -> compare (fst a) (fst b)) [(k1,v1),(key,value),(k2,v2)]
		first 	= kvs !! 0
		second	= kvs !! 1
		third	= kvs !! 2
		in (Node2 first Empty Empty, Just (second, Node2 third Empty Empty))

insertCore (Node2 (k, v) left right) (key, value)
	| key < k	= let (left', extra)	= insertCore left (key,value)
				in  if (isNothing extra)
					then (Node2 (k,v) left' right, Nothing)
					else let (newkv, newTree) = fromJust extra
						in (Node3 newkv (k,v) newTree left right, Nothing)
	| otherwise	= let (right', extra)	= insertCore right (key,value)
				in if (isNothing extra)
					then (Node2 (k,v) left right', Nothing)
					else let (newkv, newTree) = fromJust extra
						in (Node3 (k,v) newkv left right' newTree, Nothing)

insertCore (Node3 (k1,v1) (k2,v2) left middle right) (key,value)
	| key < k1	= let (left', extra)	= insertCore left (key,value)
				in if (isNothing extra)
					then (Node3 (k1,v1) (k2,v2) left' middle right, Nothing)
					else let (newkv, newTree) = fromJust extra
						in (Node2 newkv left' newTree, Just ((k1,v1), Node2 (k2,v2) middle right))
	| key < k2	= let (middle', extra)	= insertCore middle (key,value)
				in if (isNothing extra)
					then (Node3 (k1,v1) (k2,v2) left middle' right, Nothing)
					else let (newkv, newTree) = fromJust extra
						in (Node2 (k1,v1) left middle', Just (newkv, Node2 (k2,v2) newTree right))
	| otherwise	= let (right', extra)	= insertCore right (key,value)
				in if (isNothing extra)
					then (Node3 (k1,v1) (k2,v2) left middle right', Nothing)
					else let (newkv, newTree) = fromJust extra
						in (Node2 (k2,v1) left middle, Just ((k2,v2), Node2 newkv right' newTree))

insert :: (Eq k, Ord k) => (Tree23 k v) -> (k,v) -> (Tree23 k v)
insert t kv = let res = insertCore t kv
		in if (isNothing $ snd res)
			then fst res
			else let (newkv, newTree) = fromJust $ snd res
				in Node2 newkv (fst res) newTree
