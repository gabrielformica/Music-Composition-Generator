import Data.List (elemIndex)

type Evento = (Int, Int)
type Frec = Int --Frecuencia
type Contexto = (Frec, [(Evento, Frec)], [((Evento, Evento), Frec)])


type ListaProb = [(Float, Float)]


obtFrecVacia :: Contexto -> Frec
obtFrecVacia (p, _, _) = p

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
		

