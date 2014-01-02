import Tree23
import Data.Maybe

test1 :: Bool
test1 = let tree = Tree23.makeTree([
		(6,"six"),
		(5,"five"),
		(8,"ten"),
		(3,"three"),
		(1,"one")])
		in (Data.Maybe.fromJust (findTree tree 5)) == "five"
