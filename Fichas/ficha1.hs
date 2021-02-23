module Ficha1 where
import Data.Char
-- ex1--

perimetro :: Double -> Double 
perimetro r = 2*pi*r

dist' :: (Double,Double) -> (Double,Double) -> Double
dist' (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

primUlt'::[a]-> (a,a)
primUlt' x = (head x, last x)

multiplo' :: Int -> Int -> Bool
multiplo' x y = mod x y == 0

truncaImpar' :: [a] -> [a]
truncaImpar' l = if length l `mod` 2 == 0 then l else tail l

max2':: Int -> Int-> Int
max2' x y = if x>y then x else y

max3' :: Int-> Int-> Int -> Int
max3' x y z = max2'(max2' x y) z

--ex2

nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c = if (b^2-4*a*c) < 0 then 0
                else if (b^2-4*a*c) == 0 then 1
                else 2



--ex3

type Hora = (Int, Int)

testepar :: (Int, Int) -> Bool
testepar (h,m) = if h>=0 && h<=24 && m>=0 && m<=59 then True else False

comparaPar :: (Int, Int) -> (Int, Int) -> Bool
comparaPar (h1,m1)(h2,m2) = if h1>h2 then True
                            else if (h1==h2 && m1>m2) then True else False

converteH:: Hora -> Int
converteH (h,m) = h*60+m


mparah:: Int -> Hora
mparah m = (div m 60, mod m 60)

subt :: Int -> Int -> Int
subt x y = x-y

cdiferenca :: Hora -> Hora -> Int
cdiferenca (h1,m1)(h2,m2) = subt (h1*60+m1)(h2*60+m2)

addm :: Int -> Hora -> Hora
addm mi (h,m) = mparah (converteH (h,m+mi))


-- ex 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo

next x = if x == Verde then Amarelo
         else if x == Amarelo then Vermelho
         else Verde

stop :: Semaforo -> Bool
stop x = x==Vermelho

safe :: Semaforo -> Semaforo -> Bool
safe x y =  if x==Vermelho || y==Vermelho then False
            else True

-- ex 6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx point = case point of Cartesiano x _ -> x
                           Polar a b -> if a==pi/2 then 0 else b*cos a 

posy :: Ponto -> Double
posy point = case point of Cartesiano _ y -> y
                           Polar a b -> if a == pi then 0 else b*sin a

raio :: Ponto -> Double 
raio point = case point of Cartesiano x y -> sqrt (x^2 + y^2)
                           Polar a _ -> a

angulo :: Ponto -> Double
angulo point = case point of Cartesiano x y -> if x < 0 && y == 0 then pi else
                                               if x < 0 then pi + atan (y/x) else
                                               atan (y/x)
dist :: Ponto -> Ponto -> Double

dist point points = sqrt((posx point - posx points)^2 + (posy point - posy points)^2 )          

--ex 7
{-
data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo p r) = r>0
poligono (Rectangulo p1 p2) = (posx p1 /= posx p2) && (posy p1 /= posy p2)
poligono (Triangulo p1 p2 p3) = let d12 = dist p1 p2
                                    d13 = dist p1 p3
                                    d23 = dist p2 p3
                                in (d12 < d13+d23) && (d13< d12+d23) && (d23<d12+d13)

vertices :: Figura -> [Ponto]
vertices (Circulo p r) = []
vertices (Rectangulo p1 p2) = [p1,p2,Cartesiano (posx p1 posy p2), Cartesiano (posx p2 posy p1)]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3] 

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
let a = dist p1 p2
b = dist p2 p3
c = dist p3 p1
s = (a+b+c) / 2 -- semi-perimetro
in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Circulo p r) = p1*r^2
area (Rectangulo p1 p2) = let x1 = posx p1
                              x2 = posx p2
                              y1 = posy p1
                              y2 = posy p2
                          in = abs ((x1-x2)*(y1-y2))

perimetro' :: Figura -> Double
perimetro' (Circulo p r) = 2*pi*r
perimetro (Triangulo p1 p2 p3) = dist p1 p2 + dist p1 p3 + dist p2 p3
perimetro (Rectangulo p1 p2) = 2* abs (posx p2 - posx p1) + 2*abs (posy p2-posy p1)
-}
 
-- ex 8

isLower' :: Char -> Bool
isLower' x = elem x ['a'..'z']

isDigit' :: Char -> Bool
isDigit' x = elem x ['0'..'9']

isAlpha' :: Char -> Bool
isAlpha' x = elem x ['a'..'z'] || elem x ['A'..'Z']

toUpper' :: Char -> Char
toUpper' x = if isLower' x then chr (ord x -32) else x

inToDigit' :: Int -> Char
inToDigit' n = chr (n+48)

digitToInt' :: Char -> Int 
digitToInt' ch = ord ch - 48



