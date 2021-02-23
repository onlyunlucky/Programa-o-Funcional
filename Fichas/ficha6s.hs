data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

--exemplo para usar

a1 = Node 5 (Node 3 Empty (Node 4 Empty Empty)) 
            (Node 8 (Node 7 Empty Empty) (Node 9 Empty (Node 10 Empty Empty)))


altura :: BTree a -> Int
altura Empty = 0
altura (Node r Empty Empty) = 1
altura (Node r e d) = 1 + max (altura e)(altura d)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d) = contaNodos e + contaNodos d +1

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node r e d) = folhas e + folhas d

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty
prune x (Node r e d) = (Node r (prune (x-1) e) (prune (x-1) d))

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node r e d) = [r]
path (x:xs) (Node r e d) | x==True = r:path xs d
                         | x==False = r:path xs e

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = (Node r (mirror d) (mirror e))

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e1 d1) (Node y e2 d2) = (Node (f x y) (zipWithBT f e1 e2) (zipWithBT f d1 d2))
zipWithBT _ _ _ = Empty

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) e d ) = let (a1,b1,c1) = unzipBT e
                                  (a2,b2,c2) = unzipBT d
                               in (Node x a1 a2, Node y b1 b2,Node z c1 c2)


minimo :: Ord a => BTree a -> a
minimo (Node r Empty d) = r
minimo (Node r e d) = minimo e

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = (Node r (semMinimo e) d)

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty d) = (r,d)
minSmin (Node r e d) = let (m,sm) = minSmin e
                       in (m, Node r sm d)

minSmin_ :: Ord a => BTree a -> (a,BTree a)
minSmin_ (Node r Empty d) = (r,d)
minSmin_ (Node r e d) = (a,Node r b d)
        where (a,b) = minSmin_ e

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node r e d) | x<r = Node r (remove x e) d
                      | x>r = Node r e (remove x d)
                      |otherwise = aux x (Node r e d)
             where aux n (Node r e d) = case e of 
                                        Empty -> d
                                        otherwise -> case d of Empty -> e
                                        otherwise -> let (m,s) = minSmin d
                                                     in Node m e s 

type Aluno = (Numero,Nome,Regime,Classificacao) 
type Numero = Int 
type Nome = String 
data Regime = ORD | TE | MEL deriving Show 
data Classificacao = Aprov Int | Rep | Faltou deriving Show

type Turma = BTree Aluno

inscNum :: Numero -> Turma -> Bool
inscNum x Empty = False
inscNum x (Node (nu,_,_,_) e d) | x==nu = True
                                | x < nu = inscNum x e
                                | otherwise = inscNum x d
 
inscNome :: Nome -> Turma -> Bool
inscNome x Empty = False
inscNome x (Node (_,no,_,_) e d) | x==no = True
                                 | otherwise = inscNome x e || inscNome x d

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (nu,no,TE,_) e d) = (trabEst e) ++ [(nu,no)] ++ (trabEst d)
trabEst (Node _ e d) = (trabEst e) ++ (trabEst d)

nota :: Numero -> Turma -> Maybe Classificacao
nota x Empty = Nothing
nota x (Node (nu,_,_,c) e d) |x==nu = Just c
                             | x<nu = nota x e
                             | x>nu = nota x d

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas t = 100*(divide (contaFaltas t) (contaNodos t))

contaFaltas :: Turma -> Int
contaFaltas Empty = 0
contaFaltas (Node (_,_,_,Faltou) e d) = 1 + contaFaltas e + contaFaltas d

divide x y = (fromIntegral x) / (fromIntegral y)

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov t = divide (somaNotas t) (conta t)

conta :: Turma -> Int
conta Empty = 0
conta (Node (_,_,_,Aprov n) e d) = 1 + conta e + conta d
conta (Node Empty e d) = conta e + conta d

somaNotas :: Turma -> Int
somaNotas Empty = 0
somaNotas (Node (_,_,_, Aprov n) e d) = n + somaNotas e + somaNotas d
somaNotas (Node Empty e d ) =  somaNotas e + somaNotas d

-- ou numa unica travessia

mediaAprov_ :: Turma -> Float
mediaAprov_ Empty = 0
mediaAprov_ t = let (sn,n) auxs t
                in divide sn n 
auxs :. Turma -> (Int,Int)
auxs Empty = (0,0)
auxs (Node (_,_,_,Aprov n) e d) = let (sn1,n1) = auxs e
                                       (sn2,n2) = auxs d
                                   in (sn1+sn2+n,n1+n2+1)
auxs (Node Empty e d) = let (sn1,n1) = auxs e
                            (sn2,n2) = auxs d
                        in (sn1+sn2,n1+n2)






































