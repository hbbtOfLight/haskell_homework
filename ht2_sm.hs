import Data.List
data MySM a = MySM [a] deriving (Show)


newSM :: MySM a
newSM = MySM []
--  Пока не ясно как нормально это вызвать
push x (MySM xs) = MySM (x: xs)
pop (MySM (x:xs)) = (x, (MySM xs))
list (MySM xs) = xs
add (MySM (x:y:t)) = MySM((x+y):t)
sub (MySM (x:y:t)) = MySM((x-y):t)
mul (MySM (x:y:t)) = MySM((x*y):t)
sdiv (MySM (x:y:t)) = MySM((x `div` y):t)
dup (MySM (x:t)) = MySM(x:x:t)
smod (MySM (x:y:t)) = MySM((x `mod` y):t)
-- как-то так можно прописать больше операций, но есть прикол, что вызывать их по-простому не можно. Мб я не так поняла задание и это надо переписать на последовательность операций и eval