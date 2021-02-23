data ExpInt = Const Int | Simetrico ExpInt | Mais ExpInt ExpInt | Menos ExpInt ExpInt | Mult ExpInt ExpInt


calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = -(calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico e) = "-" ++ (infixa e)
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e) = (posfixa e) ++ "-"
posfixa (Mais e1 e2) = (posfixa e1) ++ (posfixa e2) ++ "+"
posfixa (Menos e1 e2) = (posfixa e1) ++ (posfixa e2) ++ "-"
posfixa (Mult e1 e2) = (posfixa e1) ++ (posfixa e2) ++ "*"


data RTree a = R a [RTree a]
-- exemplo nao funciona
ar = [R 1 [R 1 [], R 2 []], 
           R 3 [], R 4 [R 10 []]]

soma :: Num a => RTree a -> a
soma (R x lrt) = x + sum (map soma lrt)

altura :: RTree a -> Int
altura (R x []) = 1
altura (R x lrt) = 1 + maximum (map altura lrt)

prune :: Int -> RTree a -> RTree a
prune 0 (R x []) = (R x [])
prune _ (R x []) = (R x [])
prune n (R x lrt) = R x (map ( prune (n-1)) lrt)

mirror :: RTree a -> RTree a
mirror (R x lrt) = R x (reverse (map mirror lrt))

postorder :: RTree a -> [a]
postorder (R x lrt) = concat (map postorder lrt) ++ [x]


--ex3

data LTree a = Tip a | Fork (LTree a) (LTree a)

--exemplo para usar

t1 = Fork (Fork (Tip 5) (Fork (Tip 3) (Tip 2)))
          (Fork (Tip 6) (Tip 8))


ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

ltHeight :: LTree a -> Int
ltHeight (Tip x) = 1
ltHeight (Fork e d) = 1 + max (ltHeight e)(ltHeight d)

-- map aplicado a uma leafTree

mapLt :: (a->b) -> LTree a -> LTree b
mapLt f (Tip x) = Tip (f x)
mapLt f (Fork e d) = Fork (mapLt f e)(mapLt f d)


--ex4
data BTree a = Empty | Node a (BTree a) (BTree a)

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty,Tip x)
splitFTree (No y e d) = let (bt1,lt1) = splitFTree e
                            (bt2,lt2) = splitFTree d
                        in (Node y bt1 bt2, Fork lt1 lt2)

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees _ (Tip x) = Nothing
joinTrees Empty (Fork e d) = Nothing
joinTrees (Node r e d) (Fork es ds) = let ft1 = joinTrees e es
                                          ft2 = joinTrees d ds
                                      in case ft1 of
                                         Nothing -> Nothing
                                         (Just ft1) -> case ft2 of
                                                       Nothing -> Nothing
                                                       (Just ft2) -> Just (No r ft1 ft2)

--ou

joinTrees_ :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees_ (Empty) (Tip n) = Just (Leaf n)
joinTrees_ (Node e l r) (Fork a b) = Just (No e aux aux2)
    where Just aux = joinTrees l a
          Just aux2 = joinTrees r b
joinTrees_ _ _ = Nothing

















