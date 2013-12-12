module Contexto where

import Data.List (sort)
import System.Random
import Input

-- type Evento = (Int, Int)  --Evento vacio (-1,-1)
type Frec = Int --Frecuencia
type Contexto = (Frec, [(Evento, Frec)], [((Evento, Evento), Frec)])

type ProbsAcumulada = [Float]

obtFrecVacia :: Contexto -> Frec
obtFrecVacia (p, _, _) = p

obtFrecPar :: (Evento, Evento) -> Contexto -> Frec
obtFrecPar ((-1,-1), e) c = obtFrec e c
obtFrecPar e (_, _, r) =  (map snd r) !! (obtIndice e (map fst r))

obtFrec :: Evento -> Contexto -> Frec
obtFrec e (_, q, _) = (map snd q) !! (obtIndice e (map fst q))

procSecuencia :: Contexto -> [Evento] -> Contexto
procSecuencia c [] = c
procSecuencia c l = foldl agregarOrden1 (foldl agregarOrden0 c l) (creaPares l)

ordenar :: Contexto -> Contexto
ordenar (p, q, r) = (p, sort q, sort r)

creaPares :: [a] -> [(a,a)]
creaPares [] = []
creaPares (x:[]) = []
creaPares (x:y:zs) = (x,y): creaPares (y:zs)

existeEven :: Evento -> Contexto -> Bool
existeEven e (_, p, _) = e `elem` map fst p

creaOrden1 :: Evento -> Evento -> (Evento, Evento)
creaOrden1 a e = (a, e)

agregarOrden0 :: Contexto -> Evento -> Contexto
agregarOrden0 (0, [], []) a = (1, [(a, 1)], [])
agregarOrden0 (p, q, r) a = (p+1, agregar a q, r)

agregarOrden1 :: Contexto -> (Evento, Evento) -> Contexto
agregarOrden1 (p, q, r) a = (p, q, agregar a r)

agregar :: (Eq a) => a -> [(a, Frec)] -> [(a, Frec)]
agregar x [] = [(x,1)]
agregar x ((b, c):ys) =
    if x == b then
	(b, c+1):ys
    else
	(b,c): agregar x ys

obtprim :: Contexto -> Frec
obtprim (a,_,_) = a

obtprim2 :: (a,b,c) -> a
obtprim2 (a,_,_) = a

obtseg2 :: (a,b,c) -> b
obtseg2 (_,b,_) = b                   

obtter2 :: (a,b,c) -> c
obtter2 (_,_,c) = c
                  
obtseg :: Contexto -> [(Evento, Frec)]
obtseg (_,a,_) = a

obtter :: Contexto -> [((Evento,Evento), Frec)]
obtter (_,_,a) = a                 
                 
sumParFrec :: [(Frec,Frec)] -> Int
sumParFrec [] = 0
sumParFrec x = foldl (\a (b,c) -> a + (c - b)^2) 0 x

elimDups :: (Eq a) => [a] -> [a]
elimDups [] = []
elimDups (x:xs) = if notElem x xs then
                      x:elimDups xs
                  else 
                      elimDups xs


union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = elimDups [x | x <- xs ]++[y | y <- ys, notElem y xs]

buscarFrec :: (Eq a, Eq b, Num b) => a -> [(a, b)] -> b
buscarFrec a (b:bs) = if (a == (fst b)) then
                          snd b
                      else buscarFrec a bs
buscarFrec a [] = 0

constParFrec :: (Eq a) => [a] -> [(a, Frec)] -> [(a, Frec)] -> [(Frec,Frec)]
constParFrec (a:as) b c = ((buscarFrec a b),(buscarFrec a c)):constParFrec as b c
constParFrec [] _ _ = []
                      
sumCuadrados :: (Eq a) => [(a, Frec)] -> [(a, Frec)] -> Int
sumCuadrados a b = sumParFrec (constParFrec (union (map fst a) (map fst b))  a b)

calcDistancia :: Contexto -> Contexto -> Float
calcDistancia a b = sqrt  (fromIntegral ((obtprim a - obtprim b)^2 + sumCuadrados (obtseg a) (obtseg b) + sumCuadrados (obtter a) (obtter b)))


obtIndice :: (Eq a) => a -> [a] -> Int
obtIndice _ [] = -1
obtIndice x y = obtIndice' x y 0
	where obtIndice' x y acc 
		| null y = -1
		| x == head y = acc
		| otherwise = obtIndice' x (tail y) (acc +1)

obtProb :: Contexto -> (Evento, Evento) -> Float
obtProb c ((-1,-1), e)  = (obtFrec e c)  `divInt`  (obtFrecVacia c)
obtProb c@(p, q, r) (a, e) =
			 if (a, e) `elem` map fst r then
				 0.3*((obtFrec e c) `divInt` obtFrecVacia c) + 0.7*((obtFrecPar (a, e) c) `divInt` obtFrec a c)
			 else
				0.3*((obtFrec e c) `divInt` obtFrecVacia c)

divInt ::  Int -> Int -> Float
a `divInt` b = fromIntegral (a) / fromIntegral (b)

obtListaProb :: Evento -> Contexto -> [Float]
obtListaProb (-1, -1) (p, q, r) =  acumular $ normalizar $ (0.0:) $ map (`divInt` p) $ map snd q
obtListaProb e c@(p, q, r) = acumular $ (0.0:) $ normalizar $ map (obtProb c) $ map (creaOrden1 e) (map fst q)

normalizar :: [Float] -> [Float]
normalizar a = map (/sum a) a

acumular :: [Float] -> [Float]
acumular [] = []
acumular (x:[]) = []
acumular (x:y:zs) = (x+y): acumular ((x+y):zs)

obtEvenSig :: Contexto -> Float -> [Float] ->  Evento
obtEvenSig (p, q, r) a b = map fst q !! (obtIndice (head $ filter (a<=) b) b)

obtComp' :: Evento -> Contexto -> Int -> IO [Evento] 
obtComp' e c l = do
						if l <= 0 then
							return []
						else do
							p <- obtRandom
							let nuevoE = obtEvenSig c p (obtListaProb e c)
							colaE <- obtComp' nuevoE c (l -1) 
							return (nuevoE : colaE)

obtComp :: Evento -> Contexto -> Int -> [Evento] 
obtComp e c l =  if l <= 0 then 
							[]
					  else 
					  		nuevoE : obtComp nuevoE c (l-1)
	where nuevoE = (obtEvenSig c 0.4 (obtListaProb e c))

obtRandom :: IO Float
obtRandom =  getStdRandom (randomR (0.0::Float, 1.0::Float))

listify :: ([a], [b], [c]) -> [(a,b,c)]
listify ((x:xs),(y:ys),(z:zs)) = (x,y,z):listify (xs, ys, zs)
listify ([],_,_) = []


calcDistList :: [(Int, [Evento], String)] -> [Evento] -> [(Int, String, Float)]
calcDistList (x:xs) y = ((obtprim2 x),(obtter2 x) ,(calcDistancia (procSecuencia (0,[],[]) y) (procSecuencia (0,[],[]) (obtseg2 x)))):calcDistList xs y
calcDistList [] _ = [] 
