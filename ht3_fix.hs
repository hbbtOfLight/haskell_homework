import Data.Function

fgcd = fix $ \f x y ->
 if y == 0
  then x
  else (f y (x `mod` y)) 

-- fFibH = \f x y -> (f y (x + y))
-- fFib = fix $ (fFibH 0 1) 
fSumH = \f (x:t)-> 
 if null t 
  then x 
  else x + (f t)

fSum = fix $ fSumH


fReverseH :: ([a] -> [a]) -> [a] -> [a]
fReverseH _ [] = []
fReverseH f l = left ++ [head l]
  where
    left = f (tail l)

fReverse :: [a] -> [a]
fReverse = fix fReverseH

fMin :: (Ord a) => [a] -> a
fMin = fix $ \f (x:t) -> 
 if null t
 then x
 else min (f t) x

fFibNH _ _ y 0 = y
fFibNH f x y n = f y (x + y) (n - 1)

fFibN = fix fFibNH 0 1


fFibListH f x b = [b] ++ (f b (x + b)) 

fFibList = fix fFibListH 0 1





