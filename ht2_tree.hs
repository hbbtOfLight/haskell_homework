module Tree (MyTree(MyNil, MyNode), treeTraverseD, treeTraverseW) where
data MyTree a 
    = MyNil 
    | MyNode a (MyTree a) (MyTree a)

instance Show a => Show (MyTree a) where 
    show (MyNil) = "Nil"
    show (MyNode x l r) = "Node(" ++ show l ++ ") " ++ show x ++ " (" ++ show r ++ ")"

treeTraverseD :: (s -> a -> s) -> s -> MyTree a -> s

treeTraverseD _ state MyNil = state
treeTraverseD f state (MyNode a l r) =
  let state1 = treeTraverseD f state l
      state2 = f state1 a
   in treeTraverseD f state2 r

treeTraverseWH :: (s -> a -> s) -> s -> [(MyTree a)] -> s
treeTraverseWH _ state [] = state
treeTraverseWH f state (MyNil : xs) = treeTraverseWH f state xs
treeTraverseWH f state ((MyNode a l r) : xs) = treeTraverseWH f (f state a) (xs ++ [l, r])
treeTraverseW f state t = treeTraverseWH f state [t]