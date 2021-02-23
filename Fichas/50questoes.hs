enumFromto :: Int -> Int ->[Int]
enumFromto x y | x > y = []
               | x < y = x : enumFromto (x+1) y

enumFromthento :: Int -> Int -> Int -> [Int]
enumFromthento x y z | x>z || y-x < 0 = []
                     | otherwise = x : enumFromthento y (2*y-x) z

concatena :: [a] -> [a] -> [a]
concatena [] x = x
concatena y [] = y
concatena (x:xs) l = x : concatena xs l

getFromindex :: [a] -> Int -> a
getFromindex (x:xs) a = if a == 0 then x else getFromindex xs (a-1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse (h:t) = reverse' t ++ [h]

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' x (h:t) = h: take' (x-1) t

drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' _ [] = []
drop' x (h:t) = drop' (x-1) t

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):t) = (x:xs,y:ys)
     where (xs,ys) = unzip' t

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:t) = x==h || elem' x t

replicate' :: Int -> a -> [a]
replicate' 0 y = []
replicate' x y = y:replicate (x-1) y

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [h] = [h]
intersperse' x (h:t) = h:x : intersperse' x t

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = (h:takeWhile (==h) t): group' t

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

inits' :: [a] -> [[a]]
inits' [] = []
inits' [x] = [[],[x]]
inits' l = inits' (init l) ++ [l]

tails' :: [a] -> [[a]]
tails' [] = []
tails' l = [l] ++ tails' (tail l)

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' _ [] = False
isPrefixOf' [] _ = True
isPrefixOf' (h:t) (x:xs) = h==x && isPrefixOf' t xs

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' _ [] = False
isSuffixOf' [] _ = True
isSuffixOf' l h = isPrefixOf' (reverse' l) (reverse' h)


isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) (x:xs) = h==x && isSubsequenceOf' t xs || isSubsequenceOf' (h:t) xs

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x l = aux 0 x l
             where aux :: Eq a => Int -> a -> [a] -> [Int]
                   aux p _ [] = []
                   aux p x (h:t) | x == h = p: aux (p+1) x t
                                 | x /= h = aux (p+1) x t

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:t) = if x `elem` t then nub' t else x:nub' t 

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (h:t) | x==h = t
                | x/=h = h:delete' x t

remove' :: Eq a => [a] -> [a] -> [a]
remove' l [] = l
remove' [] _ = []
remove' l (h:t) = remove' (delete' h l) t

union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l
union' x [] = x
union' x (h:t) = if h `elem` x then union' x t else union' (x ++ [h]) t

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' l [] = l
intersect' (h:t) l = if h `elem` l then h:intersect' t l else intersect' t l

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) | x<h = (x:h:t)
                | x==h = (x:t)
                | x>h = h:insert' x t

unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ " " ++ unwords' t

unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) =  h ++ "\n" ++ unlines' t

pMaior' :: Ord a => [a] -> Int
pMaior' [x] = 0
pMaior' (h:t) = if h > (getFromindex t p) then 0
                else p+1
                where p = pMaior' t

temRepetidos' :: Eq a => [a] -> Bool
temRepetidos' [] = False
temRepetidos' (h:t) = if h `elem` t then True else temRepetidos' t 

algarismos' :: [Char] -> [Char]
algarismos' [] = []
algarismos' (h:t) = if h `elem` ['0'..'9'] then h: algarismos' t else algarismos' t

posImpares :: [a] -> [a]
posImpares [] = []
posImpares (h:s:t) = s:posImpares t

posPares :: [a] -> [a]
posPares [] = []
posPares (h:s:t) = h:posPares t

isSorted' :: Ord a => [a] -> Bool
isSorted' [] = True
isSorted' [x] = True
isSorted' (h:s:t) = s>=h && isSorted' (s:t) 

iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:t) = insert' h (iSort' t)

menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor l t = if length l < length t  then True else False 
 
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' a ((x,y):t) = a==x || elemMSet' a t

lengthMSet' :: [(a,Int)] -> Int
lengthMSet' l = somas 0 l
             where somas p [] = p
                   somas p ((a,b):t) = somas (p+b) t

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,y):t) = replicate' y x ++ converteMSet t

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):t) | x==a = ((a,b+1):t)
                       | otherwise = (a,b):insereMSet x t

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b):t) | x==a = t
                       | otherwise = (a,b): removeMSet x t

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)
--ou
constroiMset2 :: Ord a => [a] -> [(a,Int)]
constroiMset2 [] = []
constroiMset2 (h:t) = adiciona h (constroiMset2 t)
      where adiciona x [] = [(x,1)]
            adiciona x ((y,n):t) | x==y = (y,n+1):t
                                 | otherwise = (y,n):adiciona x t

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a:as,bs)
                   where (as,bs) = partitionEithers' t
partitionEithers' ((Right b):t) = (as,b:bs)
                   where (as,bs) = partitionEithers' t                    

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just a):t) = a:catMaybes' t
catMaybes' (Nothing:t) = catMaybes' t

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (h:t) = posicao (case h of Norte -> (x,y+1)
                                         Sul -> (x,y-1)
                                         Este -> (x+1,y)
                                         Oeste -> (x-1,y))t

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | xi > xf = Oeste :caminho (xi-1,yi) (xf,yf)
                        | xi < xf = Este  :caminho (xi+1,yi) (xf,yf)
                        | yi > yf = Sul   :caminho (xi,yi-1) (xf,yf)
                        | yi < yf = Norte :caminho (xi,yi+1) (xf,yf)
                        | otherwise = []

vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) = case h of Este -> False
                           Oeste -> False
                           _ -> vertical t

data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [Pos x y] = Pos x y
maisCentral ((Pos x y): (Pos a b):t) = if (x^2+y^2) < (a^2+b^2) then maisCentral (Pos x y:t) else maisCentral (Pos a b:t)

--ou
{-maisCentral' :: [Posicao] -> Posicao
maisCentral' = foldl (\(Pos a b) (Pos x y) -> if (a^2+ b^2) > (x^2 + y^2) then Pos x y else Pos a b)
-}

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) ((Pos a b):t) = if abs (x-a) ==1 && abs y==b || x==a && abs (y-b)==1 
                                   then Pos a b: vizinhos (Pos x y) t
                                   else vizinhos (Pos x y) t

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [Pos _ _] = True
mesmaOrdenada ((Pos x y):(Pos a b):t) = y==b && mesmaOrdenada (Pos a b:t)


data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = length [s | s <- l, case s of Vermelho -> False; _ -> True] < 2

