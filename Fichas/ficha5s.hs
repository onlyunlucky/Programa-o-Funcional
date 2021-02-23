filter_ ::  (a -> Bool) -> [a] -> [a]
filter_ teste [] = []
filter_ teste (h:t) = if (teste h) then h:filter_ teste t else filter_ teste t

map_ :: (a->b) -> [a] -> [b]
map_ f [] = []
map_ f (h:t) = (f h): map_ f t


any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (h:t) = p h || any' p t 

-- se quisermos testar para todos os elementos

all_ :: (a -> Bool) -> [a] -> Bool
all_ f [] = True
all_ f (h:t) = f h && all_ f t

zipWith_ :: (a->b->c) -> [a] -> [b] -> [c]
zipWith_ f [] _ = []
zipWith_ f _ [] = []
zipWith_ f (h:t) (x:xs) = f h x: zipWith_ f t xs

takeWhile_ :: (a->Bool) -> [a] -> [a]
takeWhile_ f [] = []
takeWhile_ f (h:t) = if f h then h: takeWhile_ f t else []

dropWhile_ :: (a->Bool) -> [a] -> [a]
dropWhile_  f  [] = []
dropWhile_ f (h:t) = if f h then dropWhile_ f t else (h:t)

span_ :: (a-> Bool) -> [a] -> ([a],[a])
span_ f [] = ([],[])
span_ f (h:t) | not (f h) = ([],h:t)
              | f h = let (le,ld) = span f t
                      in (h:le,ld)

deleteBy_ :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy_ f _ [] = []
deleteBy_ f x (h:t) | f x h = t
                    | otherwise = h: deleteBy_ f x t


sortOn_ :: Ord b => (a -> b) -> [a] -> [a]
sortOn_ f [] = []
sortOn_ f (h:t) = insert_ h (sortOn_ f t)
                 where insert_ x [] = [x]
                       insert_ x (h:t) | f x <= f h = x:h:t
                                       | otherwise = h: insert_ x t


type Polinomio = [Monomio] 
type Monomio = (Float,Int)

-- exemplo para usar [(2,3), (3,4), (5,3), (4,5)]

selgrau :: Int -> Polinomio -> Polinomio
selgrau g l = filter_ (\ (c,e) -> g==e) l

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n p = sum (map_(\(c,e)-> if n==e then 1 else 0) p) 

grau :: Polinomio -> Int 
grau l = maximum (map_ snd l)

deriv :: Polinomio -> Polinomio
deriv l = filter_ (\(c,e) -> c/=0) (map_ (\(c,e)-> (c*(fromIntegral e),e-1)) l)

calcula :: Float -> Polinomio -> Float
calcula x l = sum (map_ (\(c,e)-> c*(x^e)) l)

simp :: Polinomio -> Polinomio
simp l = filter_ (\(c,e) -> c/=0) l

mult :: Monomio -> Polinomio -> Polinomio
mult (x,s) = map_ (\(c,e) -> (c*x,e+s))

-- o mais simples

ordena :: Polinomio -> Polinomio
ordena l = sortOn_ snd l 

-- ou

ordena_ :: Polinomio -> Polinomio
ordena_ [] = []
ordena_ (h:t) = (ordena_ e) ++ [h] ++ (ordena_ d)
            where e = filter_ (\(c,e) -> e <= snd h) t
                  d = filter_ (\(c,e) -> e > snd h) t

-- processo de construçao do normaliza; recurso ao "sum" deve-se ao fato de ser 
-- necessário somar os coeficientes com o mesmo expoente, o "selgrau" para identificar os
-- monomios com o mesmo expoente, posteriomente basta juntar com os monomio que são diferentes de "e"

normaliza :: Polinomio -> Polinomio
normaliza ((c,e):ps) = (sum [cs | (cs,es) <- selgrau e ps] + c,e): normaliza [(cx,ex) | (cx,ex) <- ps, ex/= e]

soma :: Polinomio -> Polinomio -> Polinomio 
soma p1 p2 = normaliza (p1++p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto _ [] = []
produto [] _ = []
produto (m:p1) p2 = (mult m p2) ++ produto p1 p2

-- na funçao produto o "++" é usado porque mult devolve uma lista

--ou
produtof :: Polinomio -> Polinomio -> Polinomio
produtof p1 p2 = normaliza (produto p1 p2)

equiv :: Polinomio -> Polinomio -> Bool 
equiv p1 p2 = ordena_ (normaliza p1)==ordena_(normaliza p2)

--Ex3

type Mat a = [[a]]

--funçao all foi definida em cima

dimOK :: Mat a -> Bool
dimOK  (h:t) = all_ (\ y -> length y==length h) t

--ou

dimOK_ :: Mat a -> Bool
dimOK_ (l1:l2:lm) = (length l1==length l2) && dimOK_ (l2:lm)

dimMat :: Mat a -> (Int,Int)
dimMat m = (length m, length (head m))

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (l1:m1) (l2:m2) = (zipWith_ (+) l1 l2): addMat m1 m2

-- ou

addMat_ :: Num a => Mat a -> Mat a -> Mat a
addMat_ m1 m2 = zipWith_ (\ l1 l2 -> (zipWith_ (+) l1 l2 )) m1 m2 

transpose :: Mat a -> Mat a
transpose ([]:m) = []
transpose m = (map_ head m): transpose (map_ tail m)

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f (l1:m1) (l2:m2) = (zipWith_ f l1 l2): zipWMat f m1 m2
zipWMat f _ _ = []


--ou

zipWMat_ :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat_ f m1 m2 = zipWith_ (zipWith_ f) m1 m2

-- na TriSup foi necessário alterar o tipo da função

triSup :: (Eq a, Num a) => Mat a -> Bool
triSup [] = True
triSup (l:m) = all_ (==0) (map_ head m) && triSup (map_ tail m)





















