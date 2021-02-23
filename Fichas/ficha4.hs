import Ficha1
--Ex2 
--a
{-[2^x | x <- [0...10]]

--b 

[(x,y) | x<-[1...5], y<-[1...5], x+y=6]

--c


[[1...x] | x<- [1...5]]

--d

[[y^0] | y<- [1...x], x<- [1..5]]
-}

digitAlpha' :: String -> (String,String)
digitAlpha' [] = ([],[])
digitAlpha' (x:xs) = let (l,n) = digitAlpha' xs
                     in if isAlpha' x then (x:l,n) 
                     else if isDigit' x then (l,x:n)
                     else (l,n)


nzp :: [Int] -> (Int,Int,Int)
nzp l = conta l (0,0,0)
       where conta :: [Int] -> (Int,Int,Int)-> (Int,Int,Int)
             conta [] (n,z,p) = (n,z,p)
             conta (x:xs) (n,z,p) | x<0 = conta xs (n+x,z,p)
                           | x==0 = conta xs (n,z+x,p)
                           | x>0 = conta xs (n,z,p+x)

fromDigits' :: [Int] -> Int 
fromDigits' l = calcula (reverse l)
               where calcula :: [Int]->Int
                     calcula [] = 0
                     calcula (h:t) = h+10*(calcula t)


maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = calc l 0 0
               where calc :: (Num a, Ord a) => [a] -> a -> a -> a
                     calc [] m s = m
                     calc (x:xs) m s | (s+x) > m = calc xs (s+x) (s+x)
                                     | otherwise = calc xs m (s+x)


