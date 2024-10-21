{- Changhao Xu CS115 Lab2 -}
module RedBlackTree where
    
-- A color is either red or black.
data Color = Red | Black
  deriving Show

-- A red-black tree is either empty (a "leaf") or a tree node with a color,
-- two branches (both of which are red-black trees), and a value of type a.
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
  deriving Show

--Part A
-- 1 Return `True` if the given element is in the tree.
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node _ left y right) -- here color is irrelevant
  | x < y     = member x left
  | x > y     = member x right
  | otherwise = True

-- 2 Convert a tree to a list.
toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ left x right) = toList left ++ [x] ++ toList right

-- 3 Insert a new element into a tree.
insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t)
  where
    -- Insert an element into a tree.
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf  -- new nodes are colored red
    ins elem t@(Node color left elem' right)
      | elem < elem' = balance color (ins elem left) elem' right
      | elem > elem' = balance color left elem' (ins elem right)
      | otherwise = t  -- element already in the tree; no insertion required

    -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- the red and black invariants.
    balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    {- Red node has a red left child, and the left child has a red left child.
    This violates the red-black tree property that a red node can only have black children.
    To fix this, bring the red node up and make it the parent of its former parent,
    then color the new parent red and its children black. -}
    balance Black (Node Red (Node Red l1 e1 r1) e2 r2) e t =  -- case 1: left left
      Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
    {- Red node has a red left child, and the left child has a red right child.
    Similar to case 1, bring the red node up and make it the parent of its former parent,
    then color the new parent red and its children black.-}
    balance Black (Node Red l1 e1 (Node Red l2 e2 r2)) e t =  -- case 2: left right
      Node Red (Node Black l1 e1 l2) e2 (Node Black r2 e t)
    {- Red node has a red right child, and the right child has a red left child.
    Similar to case 2, bring the red node up and make it the parent of its former parent,
    then color the new parent red and its children black.-}
    balance Black l1 e1 (Node Red (Node Red l2 e2 r2) e t) =  -- case 3: right left
      Node Red (Node Black l1 e1 l2) e2 (Node Black r2 e t)
    {- Red node has a red right child, and the right child has a red right child.
    Similar to case 1, bring the red node up and make it the parent of its former parent,
    then color the new parent red and its children black.-}
    balance Black l1 e1 (Node Red l2 e2 (Node Red tl e tr)) =  -- case 4: right right
      Node Red (Node Black l1 e1 l2) e2 (Node Black tl e tr)
    
    balance color l e r = Node color l e r  -- no balancing needed

-- 4 Convert a list to a tree.
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf -- Leaf is an empty tree

-- 5
minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node _ left _ right) = 1 + min (minDepth left) (minDepth right) -- color and element are irrelevant

maxDepth :: Tree a -> Int
maxDepth Leaf = 0
maxDepth (Node _ left _ right) = 1 + max (maxDepth left) (maxDepth right) -- color and element are irrelevant

-- 6 tests the order invariant
{- Method 1: This is the inefficient method.
testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 Leaf = True
testInvariant1 (Node _ left x right) =
  all (< x) (values left) && all (> x) (values right) && testInvariant1 left && testInvariant1 right
  where
    values :: Tree a -> [a]
    values Leaf = []
    values (Node _ left x right) = values left ++ [x] ++ values right
-}
{- Method 2: This also failed because failed to consider ALL nodes, and only considered nodes at each level, and failed ot6.
testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 Leaf = True
testInvariant1 (Node _ left x right) -- node colors are irrelevant
    | (testLeft left x) && (testRight right x)
        = (testInvariant1 left) && (testInvariant1 right) -- if left and right evaluations okay, then proceed subtrees recursively
    | otherwise = False
    where
        testLeft :: Ord a => Tree a -> a -> Bool  -- nodes are strictly larger than all nodes in the left subtree
        testLeft Leaf _ = True
        testLeft (Node _ _ y _) x
          | y < x = True
          | otherwise = False

        testRight :: Ord a => Tree a -> a -> Bool -- nodes are strictly smaller than all nodes in the right subtree
        testRight Leaf _ = True
        testRight (Node _ _ y _) x
          | y > x = True
          | otherwise = False
-}
testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 Leaf = True
testInvariant1 (Node _ left x right) -- node colors are irrelevant
    | (testLeft left x) && (testRight right x)
        = (testInvariant1 left) && (testInvariant1 right) -- if left and right evaluations okay, then proceed subtrees recursively
    | otherwise = False
    where
        testLeft :: Ord a => Tree a -> a -> Bool  -- nodes are strictly larger than all nodes in the left subtree
        testLeft Leaf _ = True
        testLeft (Node _ _ y right) x             -- right value is the biggest in the left node
          | y < x = testLeft right x
          | otherwise = False

        testRight :: Ord a => Tree a -> a -> Bool -- nodes are strictly smaller than all nodes in the right subtree
        testRight Leaf _ = True
        testRight (Node _ left y _) x             -- left value is the smallest in the right node
          | y > x = testRight left x
          | otherwise = False


-- 7 tests the red invariant, no red node has a red parent
testInvariant2 :: Tree a -> Bool
testInvariant2 Leaf = True
testInvariant2 (Node Red (Node Red _ _ _) _ _) = False -- left subtree is red
testInvariant2 (Node Red _ _ (Node Red _ _ _)) = False -- right subtree is red
testInvariant2 (Node _ left _ right) = (testInvariant2 left) && (testInvariant2 right)

-- 8 tests the black invariant, all paths from the root down to any leaf have the same number of black nodes
testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
  where
    -- Given a tree, return a list of the count of black nodes on every path
    -- from the root of the tree to a leaf.
    leafCounts :: Tree a -> Int -> [Int]
    leafCounts Leaf n = [n]
    leafCounts (Node Black left _ right) n = (leafCounts left (n + 1)) ++ (leafCounts right (n + 1)) -- if black, count +1
    leafCounts (Node Red left _ right) n = (leafCounts left n) ++ (leafCounts right n)               -- otherwise, continue

    -- Return True if all the elements of a list are equal.
    allEqual :: Ord a => [a] -> Bool
    allEqual [] = True
    allEqual [_] = True
    allEqual (x:r@(y:_)) | x == y = allEqual r
                         | otherwise = False

-- Part B
-- We define Set as a type synonym for Tree.
type Set a = Tree a

-- Empty set.
empty :: Set a
empty = Leaf

-- Convert a list to a set.
toSet :: Ord a => [a] -> Set a
toSet = fromList

-- 1 takes two sets as its arguments and returns True if the first one is a subset of the second, use all
isSubset :: Ord a => Set a -> Set a -> Bool
isSubset s1 s2 = all (\x -> member x s2) (toList s1)

-- 2 takes two sets as its arguments and returns True if the two sets are equal, use isSubset
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet s1 s2 = (isSubset s1 s2) && (isSubset s2 s1)

-- 3 takes two sets as its arguments and returns a new set which is the union of the input
-- insert all elements of s1 into s2
union :: Ord a => Set a -> Set a -> Set a
union s1 s2 = foldr (\x r -> insert x r) s2 (toList s1) -- x is the current element of a list, r is the rest of the list

-- 4 takes two sets as its arguments and returns a new set which is the intersection of the input sets
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 = foldr (\x r -> if (member x s1) then insert x r else r) empty (toList s2)

-- 5 takes two sets as its arguments and returns a new set which is the set difference of the input sets
difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldr (\x r -> if not (member x s2) then insert x r else r) empty (toList s1)