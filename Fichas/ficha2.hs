module Ficha2 where
import Data.Char


dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h: dobros t

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre x (h:t) = if x==h then 1+ numOcorre x t else numOcorre x t

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h>=0 then positivos t else False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h<0 then soPos t else h:soPos t

somaNeg :: [Int] -> Int
somaNeg  [] = 0
somaNeg t = aux 0 t
         
aux x [] = x
aux x (h:t) | h<0 = aux (x+h) t
            | otherwise = aux x t

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [x] = [x]
tresUlt [x,y] = [x,y]
tresUlt [x,y,z] = [x,y,z]
tresUlt (x:xs) = tresUlt xs

segundos :: [(a,b)] -> [b] 
segundos [] = []
segundos((x,y):t) = y:segundos t 

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((x,y):t) | a==x = True
                         | otherwise = nosPrimeiros a t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = let (x,y,z) = sumTriplos t
                         in (a+x,b+y,c+z)

--ex3

isLower' :: Char -> Bool
isLower' x = elem x ['a'..'z']

isDigit' :: Char -> Bool
isDigit' x = elem x ['0'..'9']

digitToInt :: Char -> Int 
digitToInt ch = ord ch - 48

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)=if isDigit h then h:soDigitos t else soDigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if isLower x then 1+ minusculas xs else minusculas xs

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta x ((a,b):t) = if x==b then 1+ conta x t else conta x t

grau :: Polinomio -> Int
grau [(a,b)] = b
grau ((a,b):t) | b > grau t = b
               | otherwise = grau t

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau x ((a,b):t) = if x==b then (a,b):selgrau x t else selgrau x t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,xs):t) |xs/=0 = (x*(fromIntegral xs), xs-1): deriv t
                 | otherwise = deriv t

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((c,e):t) = c*x^e + calcula x t

simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,e):t) | e==0 = simp t
               | otherwise = (c,e): simp t

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (c,e) ((c1,e1):t) = (c*c1, e+e1): mult (c,e) t

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(c,e)] = [(c,e)]
normaliza ((c,e):(c1,e1):t) | e==e1 = normaliza ((c+c1,e):t)
                            | conta e t==0 = (c,e): normaliza ((c1,e1):t)
                            | otherwise = normaliza ((c,e):t ++ [(c1,e1)])

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1++p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto (h:t) p2 = (mult h p2) ++ produto t p2

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:p) = insere m (ordena p)

insere :: Monomio -> Polinomio -> Polinomio
insere m [] = [m]
insere (c,e) ((c1,e1):t) | e<e1 = ((c,e): insere (c1,e1) t)
                         | e>e1 = ((c1,e1): insere (c,e) t)
                         | e==e1 = if (c+c1)/=0 then (c+c1,e):t else t 


equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)






