{- Proyecto de Haskell

   Generacion y recuperacion de secuencias
   musicales en base a modelos de contexto.
   
   Gabriel Formica - 10-11036
   Melecio Ponte - 08-10893
-}


module Contexto where

import Data.List (sort)
import System.Random
import Input
    
{- Definicion de tipos para Frecuencia, modelo de contexto y
probabilidades acumuladas -}
type Frec = Int --Frecuencia
type Contexto = (Frec, [(Evento, Frec)], [((Evento, Evento), Frec)])
type ProbsAcumulada = [Float]

{- Funcion que devuelve la frencuencia correspondiente al contexto vacío -}
obtFrecVacia :: Contexto -> Frec
obtFrecVacia (p, _, _) = p

{- Funcion que devuelve la frecuencia de un par de eventos -}                         
obtFrecPar :: (Evento, Evento) -> Contexto -> Frec
obtFrecPar ((-1,-1), e) c = obtFrec e c
obtFrecPar e (_, _, r) =  (map snd r) !! (obtIndice e (map fst r))

{- Funcion que devuelve la frecuencia de un evento -}
obtFrec :: Evento -> Contexto -> Frec
obtFrec e (_, q, _) = (map snd q) !! (obtIndice e (map fst q))

{- Funcion que construye un modelo de contexto dada una lista de eventos -}
procSecuencia :: Contexto -> [Evento] -> Contexto
procSecuencia c [] = c
procSecuencia c l = foldl agregarOrden1 (foldl agregarOrden0 c l) (creaPares l)

{- Funcion que crea pares secuenciales con los elementos de una lista -}
creaPares :: [a] -> [(a,a)]
creaPares [] = []
creaPares (x:[]) = []
creaPares (x:y:zs) = (x,y): creaPares (y:zs)

{- Funcion que determina si un evento existe en un modelo de contexto -}
existeEven :: Evento -> Contexto -> Bool
existeEven e (_, p, _) = e `elem` map fst p

{- Funcion que crea un par de Eventos dado dos eventos -}
creaOrden1 :: Evento -> Evento -> (Evento, Evento)
creaOrden1 a e = (a, e)

{- Funcion que crea el modelo de contexto para los eventos y sus frecuencias -}
agregarOrden0 :: Contexto -> Evento -> Contexto
agregarOrden0 (0, [], []) a = (1, [(a, 1)], [])
agregarOrden0 (p, q, r) a = (p+1, agregar a q, r)

{- Funcion que crea el modelo de contexto para los pares de eventos y 
sus frecuencias -}
agregarOrden1 :: Contexto -> (Evento, Evento) -> Contexto
agregarOrden1 (p, q, r) a = (p, q, agregar a r)

{- Funcion que agrega un evento o par de eventos a una lista correspondiente -}
agregar :: (Eq a) => a -> [(a, Frec)] -> [(a, Frec)]
agregar x [] = [(x,1)]
agregar x ((b, c):ys) =
    if x == b then
	(b, c+1):ys
    else
	(b,c): agregar x ys

{- Funciones auxiliares para obtener elementos de una tupla -}
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
                 
{- Dada una lista de duplas de frecuencias, suma la resta al cuadrado de cada 
una -}
sumParFrec :: [(Frec,Frec)] -> Int
sumParFrec [] = 0
sumParFrec x = foldl (\a (b,c) -> a + (c - b)^2) 0 x

{- Funcion auxiliar que elimina elementos duplicados de una lista -}
elimDups :: (Eq a) => [a] -> [a]
elimDups [] = []
elimDups (x:xs) = if notElem x xs then
                      x:elimDups xs
                  else 
                      elimDups xs

{- Funcion auxiliar que calcula la union de dos conjuntos -}
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = elimDups [x | x <- xs ]++[y | y <- ys, notElem y xs]

{- Funcion que dado un elemento a, asociado a un elemento b, devuelve el
elemento b -}
buscarFrec :: (Eq a, Eq b, Num b) => a -> [(a, b)] -> b
buscarFrec a (b:bs) = if (a == (fst b)) then
                          snd b
                      else buscarFrec a bs
buscarFrec a [] = 0

{- Funcion que contruye pares de frecuencias dadas dos listas de duplas
elemento-frecuencia -}
constParFrec :: (Eq a) => [a] -> [(a, Frec)] -> [(a, Frec)] -> [(Frec,Frec)]
constParFrec (a:as) b c = ((buscarFrec a b),(buscarFrec a c)):constParFrec as b c
constParFrec [] _ _ = []
                   
{- Funcion que calcula la suma de cuadrados de frecuencias -}   
sumCuadrados :: (Eq a) => [(a, Frec)] -> [(a, Frec)] -> Int
sumCuadrados a b = sumParFrec (constParFrec (union (map fst a) (map fst b))  a b)

{- Funcion que calcula la distancia entre dos modelos de contexto -}
calcDistancia :: Contexto -> Contexto -> Float
calcDistancia a b = sqrt  (fromIntegral ((obtprim a - obtprim b)^2 + sumCuadrados (obtseg a) (obtseg b) + sumCuadrados (obtter a) (obtter b)))

{- Funcion que dado un elemento y una lista del tipo de ese elemento,
devuelve la posción en la lista de la primera ocurrencia -}
obtIndice :: (Eq a) => a -> [a] -> Int
obtIndice _ [] = -1
obtIndice x y = obtIndice' x y 0
	where obtIndice' x y acc 
		| null y = -1
		| x == head y = acc
		| otherwise = obtIndice' x (tail y) (acc +1)

{- Funcion que, dado un contexto y un par de enventos,
devuelve la probabilidad de dicho evento -}
obtProb :: Contexto -> (Evento, Evento) -> Float
obtProb c ((-1,-1), e)  = (obtFrec e c)  `divInt`  (obtFrecVacia c)
obtProb c@(p, q, r) (a, e) =
			 if (a, e) `elem` map fst r then
				 0.3*((obtFrec e c) `divInt` obtFrecVacia c) + 0.7*((obtFrecPar (a, e) c) `divInt` obtFrec a c)
			 else
				0.3*((obtFrec e c) `divInt` obtFrecVacia c)

{- Funcion auxiliar para la division de dos enteros en un float -}
divInt ::  Int -> Int -> Float
a `divInt` b = fromIntegral (a) / fromIntegral (b)

{- Funcion que, dado un evento y un contexto, devuelve la lista
de probabilidades acumuladas -}
obtListaProb :: Evento -> Contexto -> [Float]
obtListaProb (-1, -1) (p, q, r) =  acumular $ normalizar $ (0.0:) $ map (`divInt` p) $ map snd q
obtListaProb e c@(p, q, r) = acumular $ (0.0:) $ normalizar $ map (obtProb c) $ map (creaOrden1 e) (map fst q)

{- Funcion que normaliza un vector de flotantes -}
normalizar :: [Float] -> [Float]
normalizar a = map (/sum a) a

{- Funcion que construye una lista acumulada de flotantes -}
acumular :: [Float] -> [Float]
acumular [] = []
acumular (x:[]) = []
acumular (x:y:zs) = (x+y): acumular ((x+y):zs)

{- Funcion que, dado un contexto, una probabilidad y una lista de probabilidades
acumuladas, devuelve el siguiente evento generado -}
obtEvenSig :: Contexto -> Float -> [Float] ->  Evento
obtEvenSig (p, q, r) a b = map fst q !! (obtIndice (head $ filter (a<=) b) b)

{- Funcion que genera una nueva composicion dado un evento y un contexto -}
obtComp' :: Evento -> Contexto -> Int -> IO [Evento] 
obtComp' e c l = do
						if l <= 0 then
							return []
						else do
							p <- obtRandom
							let nuevoE = obtEvenSig c p (obtListaProb e c)
							colaE <- obtComp' nuevoE c (l -1) 
							return (nuevoE : colaE)

{- Funcion que obtiene un numero random entre 0 y 1 -}
obtRandom :: IO Float
obtRandom =  getStdRandom (randomR (0.0::Float, 1.0::Float))

{- Funcion auxiliar que crea una lista de tripletas dada una tripleta de listas -}
listify :: ([a], [b], [c]) -> [(a,b,c)]
listify ((x:xs),(y:ys),(z:zs)) = (x,y,z):listify (xs, ys, zs)
listify ([],_,_) = []

{- Funcion que calcula las distancias para una lista de cadenas de eventos con 
respecto a una cadena de eventos en particular -}
calcDistList :: [(Int, [Evento], String)] -> [Evento] -> [(Int, String, Float)]
calcDistList (x:xs) y = ((obtprim2 x),(obtter2 x) ,(calcDistancia (procSecuencia (0,[],[]) y) (procSecuencia (0,[],[]) (obtseg2 x)))):calcDistList xs y
calcDistList [] _ = [] 
