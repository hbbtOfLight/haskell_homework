xgcd a b
    | b == 0    = a
    | otherwise = xgcd b (a `mod` b)

xpow x 0 = 1
xpow x y
    | even y    = a * a
    | otherwise = x * a * a
    where
        a = xpow x (y `div` 2)

matMul :: [[Integer]] -> [[Integer]] -> [[Integer]]
matMul a b = [[c11, c12], [c21, c22]]
  where
    c11 = (a !! 0 !! 0) * (b !! 0 !! 0) + (a !! 0 !! 1) * (b !! 1 !! 0)
    c12 = (a !! 0 !! 0) * (b !! 0 !! 1) + (a !! 0 !! 1) * (b !! 1 !! 1)
    c21 = (a !! 1 !! 0) * (b !! 0 !! 0) + (a !! 1 !! 1) * (b !! 1 !! 0)
    c22 = (a !! 1 !! 0) * (b !! 0 !! 1) + (a !! 1 !! 1) * (b !! 1 !! 1)
xfibHelper 1 = [[0, 1], [1, 1]]
xfibHelper y
    | even y    = matMul a a
    | otherwise = matMul mat [[0, 1], [1, 1]]
    where
        a = xfibHelper (y `div` 2)
        mat = matMul a a

xFib 0 = 0
xFib a = (xfibHelper a) !! 0 !! 1

findDiv :: Integer -> Integer -> Integer
findDiv start x
    | start >= x  = x
    | x `mod` start == 0 = start
    | otherwise = findDiv (start + 1) x


xDivsSum :: Integer -> Integer -> Integer -> Integer
xDivsSum sum lastdiv x
    | lastdiv == x = sum
    | otherwise =  xDivsSum (sum + lastdiv) (findDiv (lastdiv + 1) x) x

isPerfect :: Integer -> Bool

isPerfect 1 = True
isPerfect x = (x == (xDivsSum 0 1 x))

xSiracoseHelper len 1 = len + 1
xSiracoseHelper len x
    | x `mod` 2 == 0 = xSiracoseHelper (len + 1) (x `div` 2)
    | otherwise = xSiracoseHelper (len + 1) (x * 3 + 1)

xSiracose x = xSiracoseHelper 0 x

-- copy-paste from lk
xReverseHelper r [] = r
xReverseHelper r (x:xs) = xReverseHelper (x:r) xs

xReverse l = xReverseHelper [] l

evalPolynomial :: [Integer] -> Integer -> Integer
evalHelper :: [Integer] -> Integer -> Integer -> Integer -> Integer
evalHelper [] val _ _ = val
evalHelper (cf:cfs) val cx x = evalHelper cfs (val + cf * cx) (cx * x) x

evalPolynomial coeffs x = evalHelper c 0 1 x where 
    c = xReverse coeffs

dellanoy x 0 = 1
dellanoy 0 y = 1
dellanoy x y = dellanoy x (pred y) + dellanoy (pred x) (pred y) + dellanoy (pred x) y

cloneHelper :: [a] -> [a] -> Integer -> Integer -> [a]
cloneHelper state [] _ _ = state
cloneHelper state src 0 init_turns = cloneHelper state (tail src) init_turns init_turns
cloneHelper state src turns init_turns = cloneHelper (state ++ [head(src)])  src (pred turns) init_turns
clone :: [a] -> Integer -> [a]
clone src turns = cloneHelper [] src turns turns

xZipWith f [] _ = []
xZipWith f _ [] = []
xZipWith f (x : xt) (y : yt) = [f(x, y)] ++ (xZipWith f xt yt)
