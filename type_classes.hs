
module MySetMod (MySet, MyFlagSet, MyListSet) where 
class MySet a where
    contains :: a -> Int -> Bool
    tolist :: a -> [Int]
    fromlist :: [Int] -> a
    intersection :: a -> a -> a
    union :: a -> a -> a
    difference :: a -> a -> a

newtype MyListSet = MyListSet [Int]
    deriving (Show)

newtype MyFlagSet = MyFlagSet [Bool] 
    deriving (Show)


-- orderedIntersectionHelper :: Integer a => [a] -> [a] -> [a] -> [a]
orderedIntersectionHelper _ [] inters = inters
orderedIntersectionHelper [] _ inters = inters
orderedIntersectionHelper (x:as) (b:bs) inters
    | x == b  = orderedIntersectionHelper as bs (inters ++ [x])
    | x > b   = orderedIntersectionHelper (x:as) bs inters
    | otherwise = orderedIntersectionHelper as (b:bs) inters

-- orderedUnionHelper :: Ord a => [a] -> [a] -> [a] -> [a]
orderedUnionHelper x [] u = u ++ x
orderedUnionHelper [] b u = u ++ b
orderedUnionHelper (x:as) (b:bs) u
    | x < b  = orderedUnionHelper as (b:bs) (u ++ [x])
    | x == b = orderedUnionHelper as bs (u ++ [x])
    | otherwise = orderedUnionHelper (x:as) bs (u ++ [b])

-- orderedDifferenceHelper :: Ord a => [a] -> [a] -> [a] -> [a]
orderedDifferenceHelper [] _ u = u
orderedDifferenceHelper x [] u = u ++ x
orderedDifferenceHelper (x:xs) (y:ys) u
    | x == y = orderedDifferenceHelper xs ys u
    | x < y  = orderedDifferenceHelper xs (y:ys) (u ++ [x])
    | otherwise = orderedDifferenceHelper (x:xs) ys u

-- toListHelper :: [a] -> Integer -> [Integer] -> [Integer]
toListHelper [] _ l = l
toListHelper (x:xs) idx l 
    | x == True = toListHelper xs (idx + 1) (l ++ [idx])
    | otherwise = toListHelper xs (idx + 1) l

instance MySet MyListSet where
    contains (MyListSet a) v = 
        case a of
            [] -> False
            (x:xs) | x == v -> True
            _:xs -> contains (MyListSet xs) v
    tolist (MyListSet a) = a
    fromlist lst = MyListSet lst
    intersection (MyListSet a) (MyListSet b) = MyListSet (orderedIntersectionHelper a b [])
    union (MyListSet a) (MyListSet b) = MyListSet (orderedUnionHelper a b [])
    difference (MyListSet a) (MyListSet b) = MyListSet (orderedDifferenceHelper a b [])


fromListHelper [] _ l = l
fromListHelper (a:as) i l 
    | a == i = fromListHelper as (i + 1) (l ++ [True])
    | otherwise = fromListHelper (a:as) (i + 1) (l ++ [False])


boolInter [] a l = (l ++ a)
boolInter a [] l = (l ++ a)
boolInter (a: as) (b: bs) l = boolInter as bs (l ++ [a && b])

boolUni [] a l = (l ++ a)
boolUni a [] l = (l ++ a)
boolUni (a: as) (b: bs) l = boolUni as bs (l ++ [a || b])

boolDiff [] a l = l
boolDiff a [] l = (l ++ a)
boolDiff (a: as) (b: bs) l = boolDiff as bs (l ++ [a && (not b)])

instance MySet MyFlagSet where
    contains (MyFlagSet a) v = (length a) > v && (a !! v)
    tolist (MyFlagSet a) = toListHelper a 0 []
    fromlist lst = MyFlagSet (fromListHelper lst 0 [])
    intersection (MyFlagSet a) (MyFlagSet b) = MyFlagSet (boolInter a b []) 
    union (MyFlagSet a) (MyFlagSet b) = MyFlagSet (boolUni a b []) 
    difference (MyFlagSet a) (MyFlagSet b) = MyFlagSet (boolDiff a b []) 

