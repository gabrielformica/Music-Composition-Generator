import System.Random
import Data.List

type Evento = (Int, Int)  --Evento vacio (-1,-1)
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

procSecuencia :: [Evento] -> Contexto -> Contexto
procSecuencia [] c = c
procSecuencia l c = foldl agregarOrden1 (foldl agregarOrden0 c l) (creaPares l)

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

obtIndice :: (Eq a) => a -> [a] -> Int
obtIndice _ [] = -1
obtIndice x y = obtIndice' x y 0
	where obtIndice' x y acc 
		| null y = -1
		| x == head y = acc
		| otherwise = obtIndice' x (tail y) (acc +1)

obtProb :: Contexto -> (Evento, Evento) -> Float
obtProb c ((-1,-1), e)  = (fromIntegral (obtFrec e c))  / fromIntegral (obtFrecVacia c)
obtProb c@(p, q, r) (a, e) =
			 if (a, e) `elem` map fst r then
				 0.3*((obtFrec e c) `divInt` obtFrecVacia c) + 0.7*((obtFrecPar (a, e) c) `divInt` obtFrec a c)
			 else
				0.0

divInt ::  Int -> Int -> Float
a `divInt` b = fromIntegral (a) / fromIntegral (b)


obtListaProb :: Evento -> Contexto -> [Float]
obtListaProb (-1, -1) (p, q, r) =  acumular $ (0.0:) $ map (`divInt` p) $ map snd q
obtListaProb e c@(p, q, r) = acumular $ (0.0:) $  map (obtProb c) $ map (creaOrden1 e) (map fst q)

acumular :: [Float] -> [Float]
acumular [] = []
acumular (x:[]) = []
acumular (x:y:zs) = (x+y): acumular ((x+y):zs)


