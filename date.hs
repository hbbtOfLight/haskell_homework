
module DateModule (MyDate) where
-- import Data.Char (isDigit)
data MyDate = MyDate
  { y :: Int
  , m :: Int
  , d :: Int
  }

instance Show MyDate where 
    show (MyDate y m d) = show y ++ ":" ++ show m ++ ":" ++ show d

instance Eq MyDate where
    (==) (MyDate y m d) (MyDate y1 m1 d1) = y == y1 && m == m1 && d == d1
    (/=) (MyDate y m d) (MyDate y1 m1 d1) = y /= y1 || m /= m1 || d /= d1

instance Ord MyDate where
    compare (MyDate y m d) (MyDate y1 m1 d1) = [y, m, d] `compare` [y1, m1, d1]

   -- (<) (MyDate y m d) (MyDate y1 m1 d1) = y < y1 || y == y1 && m < m1 || y == y1 && m == m1 && d < d1
   -- (>) (MyDate y m d) (MyDate y1 m1 d1) = [y, m, d] compare [y1, m1, d1]

instance Read MyDate where
    readsPrec _ input = [(MyDate (read y) (read m) (read d), "")]
        where
            parts = words input
            [y, m, d] = parts

(+.) (MyDate y m d) (n) = MyDate (y + y1) (m + m1) (d + d1)
        where 
            y1 = n `div` 360
            m1 = n `mod` 360 `div` 30
            d1 = n `mod` 30

(-.) (MyDate y m d) (MyDate y1 m1 d1) = y * 360 + m * 30 + d - y1 * 360 - m1 * 30 - d1



