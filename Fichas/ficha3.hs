data Hora = H Int Int deriving Show

type Etapa = (Hora,Hora) 
type Viagem = [Etapa]

-- funçoes Ficha1

horaValida :: Hora -> Bool
horaValida (H h m) = elem h [0..23] && elem m [0..59]

(H h m) `horaDepois` (H h2 m2) = (h,m) >= (h2,m2)

hor2min (H h m) = 60 * h + m

min2hor min = H (div min 60) (mod min 60)

hordiff h1 h2 = min2hor $ abs $ hor2min h1 - hor2min h2

addmins hor min = min2hor $ mod (hor2min hor + min) 1440

--ex1

etapaTeste :: Etapa -> Bool
etapaTeste (h1,h2) = horaValida (h1) && horaValida (h2) && h2 `horaDepois` h1

testaViagem :: Viagem -> Bool
testaViagem (e:[]) = etapaTeste e
testaViagem ((h1,h2):(h3,h4):t)= etapaTeste (h1,h2) && etapaTeste (h2,h3) && testaViagem ((h3,h4):t)

partidaChegada :: Viagem -> Etapa
partidaChegada [(h1,h2)] = (h1,h2)
partidaChegada [(h1,h2),(h3,h4)] = (h1,h4)
partidaChegada ((h1,h2):(h3,h4):t) = partidaChegada ((h1,h2):t)

tempoViagem :: Viagem -> Hora
tempoViagem [(h1,h2)] = hordiff h1 h2
tempoViagem ((h1,h2):t) = addmins (hordiff h1 h2) (hor2min (tempoViagem t))

tempoDeEspera :: Viagem -> Hora
tempoDeEspera [(h1,h2)] = H 0 0
tempoDeEspera ((h1,h2):(h3,h4):t) = addmins (hordiff h2 h3) (hor2min (tempoDeEspera ((h3,h4):t)))

tempoTotalViagem :: Viagem -> Hora
tempoTotalViagem v = addmins (tempoViagem v) (hor2min (tempoDeEspera v))

--ex3

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving Show
{-
type Nome = String 
type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n em [] = [(n,[Email em])]
acrescEmail n em ((n1,lc):ag) | n==n1 = ((n1,(Email em):lc):ag)
                              | n/=n1 = (n1,lc): acrescEmail n em ag

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((n1,lc):ag) | n==n1 = Just (getEmail lc)
                         | n/=n1 = verEmails n ag
                    where getEmail :: [Contacto] -> [String]
                          getEmail [] = []
                          getEmail ((Email em):lc) = em: getEmail lc
                          getEmail ((_):lc) = getEmail lc

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa t):lc) = t: consTelefs lc
consTelefs ((Trab t):lc) = t:consTelefs lc
consTelefs ((Tlm t):lc) = t:consTelefs lc
consTelefs ((_):lc)= consTelefs lc

consTelefs' :: [Contacto] -> [Integer]
consTelefs' [] = []
consTelefs' (x:lc) = case x of (Casa t) -> t:consTelefs' lc
                               (Trab t) -> t:consTelefs' lc
                               (Tlm t) -> t:consTelefs' lc
                               _ -> consTelefs' lc



casa :: Nome -> Agenda -> Maybe Integer
casa n [] = Nothing
casa n ((n1,lc):ag) |n/=n1 = casa n ag
                    | n==n1 = getTelcasa lc
                 where getTelcasa :: [Contacto] -> Maybe Integer
                       getTelcasa [] = Nothing
                       getTelcasa ((Casa t):lc) = Just t
                       getTelcasa ((_):lc) = getTelcasa lc
-}

--ex4

type Dia = Int 
type Mes = Int 
type Ano = Int 
type Nome = String

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome,Data)]

procura :: Nome -> TabDN -> Maybe Data
procura n [] = Nothing
procura n ((n1,d):tb) | n==n1 = Just d
                      | otherwise = procura n tb

idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D d m a) n t = let d1 = procura n t
                      in case d1 of Nothing -> Nothing
                                    Just (D d1 m1 a1) -> if (m,d) >= (m1,d1) then Just (a-a1)
                                                         else Just (a-a1+1)
                      
anterior :: Data -> Data -> Bool
anterior (D d m a) (D di ms an) = (a,m,d) <= (an,ms,di)

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):tb) = insere (n,d) (ordena tb)
                  where insere :: (Nome, Data) -> TabDN -> TabDN
                        insere _ [] = []
                        insere (n,d) ((n1,d1):tb) | d<=d1 = ((n,d):(n1,d1):tb)
                                                  | otherwise (n1,d1) : insere (n,d) t
{-
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D d m a) tabela = (n,idade) : porIdade (D d m a) ts
    where ((n,D dx mx ax):ts) = ordena tabela
          idade = if m > mx || mx == m && d > dx then (a - ax) else ((a - ax) - 1)
-}
--ex5
data Movimento = Credito Float | Debito Float deriving Show

data Data = D Int Int Int deriving Show

data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext x []) _ = []
extValor (Ext x ((_,_,m):ls)) valor = case m of Credito n -> if n>=valor then m:extValor (Ext x ls) valor else extValor (Ext x ls) valor
                                                Debito n -> if n>=valor then m:extValor (Ext x ls) valor else extValor (Ext x ls) valor

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext x [_]) _ = []
filtro (Ext x (d,info,mov):ls)) ds = if info `elem` ds then (d,mov):filtro (Ext x ls) ds else filtro (Ext x ls) ds

creDeb :: Extracto -> (Float,Float)
creDeb (Ext x lm) = separa lm
        where separa :: [(Data,String,Movimento)] -> (Float,Float)
              separa [] = (0,0)
              separa ((_,_,mov):ls) = let (sc,sd) = separa lm
                                      in case mov of Credito x -> (sc+x,sd)
                                                     Debito x -> (sc,sd+x) 
-- ou

creDeb' :: Extracto -> (Float,Float)
creDeb' (Ext vi lm) = separa' lm (0,0)
separa' :: [(Data,String,Movimento)] -> (Float,Float) -> (Float,Float)
separa' [] (sd,sc) = (sd,sc)
separa' ((_,_,mov):ls) (sd,sc) = case mov of Debito x -> (sd+x,sc)
                                             Credito x -> (sd,sc+x)

saldo :: Extracto -> Float
saldo (Ext vi lm) = let (sd,sc) = creDeb (Ext vi lm)
                    in vi + sc-sd







