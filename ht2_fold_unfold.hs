import Data.List

------------------------------unfold to less---------------------------
xLessGen x
  | x > 0  = Just(x, x - 1)
  |otherwise = Nothing

xLessUnfold x = unfoldr xLessGen (x - 1)

--------------------------------fold/unfold to bin ---------------------
xDoubleGen x
  | x == 0  = Nothing
  | x `mod` 2 == 1   = Just(1, (x `div` 2))
  | otherwise = Just(0, (x `div` 2))

revXDiv2Unfold x = reverse (unfoldr xDoubleGen x)
xDiv2Unfold x = unfoldr xDoubleGen x

xTo10 x = foldl (\x y -> x * 2 + y) 0 x

------------------------unfold to siracose-----------------------

xSiracose x
    | x == 0  = Nothing
    | x == 1 = Just(x, 0)
    | x `mod` 2 == 0 = Just (x, (x `div` 2))
    | otherwise = Just (x, (x * 3 + 1))

xUnfoldSir x = unfoldr xSiracose x

-------------------------unfold to primes-------------------------------
-- copy-paste old ht

findDiv :: Integer -> Integer -> Integer
findDiv start x
    | start >= x  = x
    | x `mod` start == 0 = start
    | otherwise = findDiv (start + 1) x


go start x 
    | x < 2  = Nothing
    | x `mod` start == 0 = Just(start, [start, x `div` start])
    | otherwise =  go (findDiv start x) x

xPrimeGen state =  go (state !! 0) (state !! 1)

xUnfoldPrime x = unfoldr xPrimeGen [2, x]

------------------Fibonacci------------------------
xFHelper prev curr n 
    | n > 0 && curr + prev > n = Nothing
    | n < 0 || curr + prev <= n = Just(prev + curr, [curr, prev + curr, n])


xFGen state = xFHelper (state !! 0) (state !! 1) (state !! 2)

xUnfoldFibInf = unfoldr xFGen [0, 1, -1]
xUnfoldFib n = unfoldr xFGen [0, 1, n]

----------------------------primes-------------------
xPrimeChecker [] _  = True
xPrimeChecker (x:primes) y 
    | (y `mod` x) == 0 = False
    | otherwise = xPrimeChecker primes y


xPrimeHelper :: [Integer] -> Integer -> Maybe (Integer, [Integer])
xPrimeHelper prev_primes start 
    | (xPrimeChecker prev_primes start) == True = Just(start, prev_primes ++[start])
    | otherwise = xPrimeHelper prev_primes (start + 1)

xPrimeEGen [] = Just(2, [2])
xPrimeEGen prev_primes = xPrimeHelper prev_primes ((last prev_primes) + 1)

xPrimeUnfold n = takeWhile (\x -> x <= n) (unfoldr xPrimeEGen [])
xPrimeUnfoldInf = unfoldr xPrimeEGen []