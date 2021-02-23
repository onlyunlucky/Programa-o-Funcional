import Data.List
import Data.Char

data Frac = F Integer Integer


--ex1 primeiro fazer o mdc
{-
mdc :: Integer -> Integer -> Integer
mdc x y | x==y =x
        | x<y = mdc x (y-x)
        | x>y = mdc (x-y) y

normaliza :: Frac -> Frac
normaliza (F _ 0) = error " fracção invalida"
normaliza (F 0 _) = F 0 1
normaliza (F n d) = let n' = abs n
                        d' = abs d
                        s = if (n*d)>0 then 1 else -1
                        k = mdc n' d'
                    in F (s* (n' `div` k) (d' `div` k))

instance Eq Frac where
    (F n1 d1)==(F n2 d2) = (n1*d2)==(n2*d1)

instance Show Frac where
    show (F n d) = show n ++ " / " ++ show d

instance Ord Frac where
    compare (F n d)(F n1 d1)| n*d1 < n1*d = LT
                            | n*d1 > n1*d = GT
                            | n*d1 == n1*d = EQ

instance Num Frac where
    (+) (F n d)(F n1 d1) = normaliza (F(n*d1 + n1*d) (d1+d))
    (-) (F n d)(F n1 d1) = normaliza (F(n*d1 - n1*d) (d1*d))
    (*) (F n d)(F n1 d1) = normaliza (F(n*n1) (d*d1))
    negate (F n d) = normaliza (F (-n) d)
    abs (F n d) = F (abs n) (abs d)
    signum (F n d) = F (signum (n+d)) 1
    fromInteger x = F x 1

--f

maioresDobro :: Frac -> [Frac] -> Frac
maioresDobro f lf = filter (\ f1 -> f1 > (2*f)) lf

--ou 
maioresDobro_ :: Frac -> [Frac] -> Frac
maioresDobro_ f lf = filter (> (2*f)) lf

--ex2

data Exp a = Const a 
           | Simetrico (Exp a) 
           | Mais (Exp a) (Exp a) 
           | Menos (Exp a) (Exp a) 
           | Mult (Exp a) (Exp a)

-- fomos buscar funçoes da ficha 7, basta substituir "ExpInt" por "Exp a"

calcula ::Num a => Exp a -> a
calcula (Const e) = e
calcula (Simetrico e) = -(calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

infixa :: Exp a -> String
infixa (Const e) = show e
infixa (Simetrico e) = "-" ++ (infixa e)
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

--instance Show a => Show (Exp a) where
    --show e = infixa e

--ou se quisermos fazer tudo

instance Show a => Show (Exp a) where
	show (Cons a) = show a
	show (Simetrico a) = "( - " ++ show a ++ ")"
	show (Mais a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
	show (Menos a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
	show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"

instance (Num a, Eq a) => Eq (Exp a) where
	e1==e2 = (Calcula e1)==(Calcula e2)

instance (Num a) => Num (Exp a) where
	(+) e1 e2 = Const (calcula e1 + calcula e2)
	(-) e1 e2 = Const (calcula e1 - calcula e2)
    (*) e1 e2 = Const (calcula e1 * calcula e2)
    signum e = Const (signum (calcula e))
    abs e = Const (abs(calcula e))
    negate e = Const (negate (calcula e))
    fromInteger e = Const (fromInteger x)

--extra definir instance Ord

instance (Num a, Ord a)=> Ord (Exp a) where
compara e1 e2 |(calcula e1)>(calcula e2) = GT
              |(calcula e1)<(calcula e2) = LT
              |(calcula e1)==(calcula e2) = EQ


--ex3
-}
data Movimento = Credito Float | Debito Float 
data Data = D Int Int Int 
data Extracto = Ext Float [(Data, String, Movimento)]

instance Ord Data where
compare (D d m a)(D d1 m1 a1) |(a,m,d)<(a1,m1,d1) = LT
                              |(a,m,d)==(a1,m1,d1) = EQ
                              |(a,m,d)>(a1,m1,d1) = GT

--ao definirmos a instance ORd temos que definir a EQ

instance Eq Data where
    (D d m a)==(D d1 m1 a1) = d==d1 && m==m1 && a==a1

instance Show Data where
    show (D d m a)= show a ++ "/" ++ show m ++ "/" ++ show d 


--c)

ordena :: Extracto -> Extracto
ordena (Ext si lm) = Ext si (sortOn (\(a,b,c) -> d) lm)


--d)

instance Show Extracto where
show (Ext si lm) = 
	" Saldo anterior:" ++ show si ++ "\n" ++ replicate (4*12) '-' ++ "\n" ++ col "Data" 12 ++ col "Descriçao" 12 ++ col "Credito" 12 ++ col "Debito" 12 ++ "\n" ++ replicate (4*12) '-' ++ "\n" ++ listaMov lm ++ "\n" ++ replicate (4*12) '-' ++ "\n" ++ "Saldo Actual:" ++ show (saldo (Ext si lm))

col :: String -> Int -> String
col s n = take n (s++ replicate n ' ')

listaMov :: [(Data, String, Movimento)] -> String
listaMov [] = []
listaMov ((d,desc,Credito x):lm) = col (show d) 12 ++ col desc 12 ++ col (show x) 12 ++ "\n" ++ listaMov lm
listaMov ((d,desc,Debito x):lm) = col (show d) 12 ++ col desc 12 ++ col (show x) 12 ++ "\n" ++ listaMov lm

saldo :: Extracto -> Float
saldo (Ext si lm) = si + sc-sd
                  where (sc,sd) = somaCreDeb lm
somaCreDeb :: [(Data, String, Movimento)] -> (Float,Float)
somaCreDeb [] = (0,0)
somaCreDeb ((_,_,m):lm) = let (sc,sd) = somaCreDeb lm
                          in case m of
                          	      Debito x -> (sc,sd+m)
                          	      Credito x-> (sc+m,sd)


