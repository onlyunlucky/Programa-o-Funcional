import System.Random
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Control.Monad

-- 1a)

bingo :: IO()
bingo = do ln <- geraNum []; 
           print ln;

geraNum :: [Int] -> IO [Int]
geraNum  l = do if (length l==90) then return l
                else do k <- randomRIO (0,90)
                        if k `elem` l then geraNum l
                            else geraNum (k:l)

geraDig :: IO [Int]
geraDig = do n1<- randomRIO(0,9);
             n2<- randomRIO(0,9);
             n3<- randomRIO(0,9);
             n4<- randomRIO(0,9);
             return[n1,n2,n3,n4]


leDig :: IO [Int]
leDig = do 
        putStrLn "insira 4 digitos separados por espaços e termine com ENTER:";
        s <- getLine
        let lnj = map (digitToInt.head) (words s)
        return lnj

comuns :: [Int] -> [Int] -> [Int]
comuns (x:xs) ys
  | elem x ys = x: comuns xs (ys\\[x])
  | otherwise = comuns xs ys
comuns [] _ = []

-- map (digitToInt.head) (words s) é igual a = map digitToInt (map head (words s))

mastermind :: IO ()
mastermind = do 
             putStrLn "Inicio";
             lng <- geraDig
             lnj <- leDig
             if lng==lnj then putStrLn "Ganhou!"
             else do
                     let ldpc = concat (zipWith (\x y -> if x==y then [x] else []) lng lnj);
                     let lng' = lng\\ldpc;
                     let lnj' = lnj\\ldpc;
                     let ldpe = comuns lng' lnj';
                     putStrLn ("Digitos certos nas posicoes certas" ++ show (length ldpc))
                     putStrLn ("Digitos certos nas posicoes certas" ++ show (length ldpe))

-- ou

mastermind' :: IO ()
mastermind' = do 
               putStrLn "Inicio"
               lng <- geraDig;
               joga lng

joga :: [Int] -> IO()
joga lng = do
             lnj <- leDig
             if lng==lnj then putStrLn "Ganhou!"
             else do
                     let ldpc = concat (zipWith (\x y -> if x==y then [x] else []) lng lnj);
                     let lng' = lng\\ldpc;
                     let lnj' = lnj\\ldpc;
                     let ldpe = comuns lng' lnj';
                     putStrLn ("Digitos certos nas posicoes certas" ++ show (length ldpc))
                     putStrLn ("Digitos certos nas posicoes certas" ++ show (length ldpe))
                     joga lng;

-- ex: 2

aps = Ap [2,5,35,40,1] (2,12)
apx = Ap [2,5,35,40,1] (2,12)


data Aposta = Ap [Int] (Int,Int)

valida :: Aposta -> Bool
valida (Ap l (x,y)) = length l == 5 && all (\x-> elem x [1..50]) l &&
                      (nub l) == l && all (\x-> elem x [1..12]) [x,y] && (x/=y)

comunsAposta :: Aposta -> Aposta -> (Int,Int)
comunsAposta (Ap l (x,y)) (Ap n (z,w)) = (length (intersect l n), length (intersect [x,y] [z,w])) 

instance Eq Aposta where
 (==) ap1 ap2 = comunsAposta ap1 ap2 == (5,2)

premio :: Aposta -> Aposta -> Maybe Int
premio ap1 ap2 = case (comunsAposta ap1 ap2) of
                 (5,2) -> Just 1
                 (5,1) -> Just 2
                 (5,0) -> Just 3
                 (4,2) -> Just 4
                 (4,1) -> Just 5
                 (4,0) -> Just 6
                 (3,2) -> Just 7
                 (2,2) -> Just 8
                 (3,1) -> Just 9
                 (3,0) -> Just 10
                 (1,2) -> Just 11
                 (2,1) -> Just 12
                 (2,0) -> Just 13
                 _     -> Nothing

geraListaNum :: [Int] -> Int -> (Int,Int) -> IO [Int]
geraListaNum l n (i,s) = do if (length l) == n then return l
	                        else do k <- randomRIO (i,s)
	                                if elem k l then geraListaNum l n (i,s)
	                                	else geraListaNum (k:l) n (i,s)


geraAposta :: IO Aposta
geraAposta = do ln<-geraListaNum [] 5 (1,50);
                [x,y] <-geraListaNum [] 2 (1,9)
                return (Ap ln (x,y))

leAposta :: IO Aposta
leAposta = do 
           putStrLn "insira 5 numeros entre 1 e 50, separados por espaços e termine com ENTER:";
           s <- getLine
           let ln = map read (words s)
           putStrLn " insira 2 numeros entre 1 e 12, separados por espaços e termine com ENTER:";
           s1 <- getLine;
           let le = map read (words s1)
           let ap = (Ap ln (x,y));
           if valida ap then return ap
           else do 
           putStrLn "aposta invalida";
           leAposta;


joga' :: Aposta -> IO()
joga' apg = do 
            apj <- leAposta;
            let p = premio apg apj;
            putStrLn mostraPremio p;

mostraPremio :: Maybe Int -> String
mostraPremio Nothing = "Sem premio!"
mostraPremio (Just p) = "Recebeu premio:" ++ show p


{-

main :: IO () 
main = do ch <- geraChave 
          ciclo ch

menu :: IO String menu = do { putStrLn menutxt 
                            ; putStr "Opcao:" 
                            ; c <- getLine  
                            ; return c 
                            }
                            where menutxt = unlines ["", 
                                                     "Apostar ........... 1", 
                                                     "Gerar nova chave .. 2", "",
                                                     "Sair .............. 0"]

ciclo :: Aposta -> IO()
ciclo ap = do op <- menu
              case op of
                      "1" -> do {joga ap; ciclo ap}
                      "2" -> do {ch<- geraChave; ciclo ch}
                      "0" -> putStrLn "Fim"
                       _  -> ciclo ap

-}









