type Evento = (Int, Int)
type Frec = Int --Frecuencia
type Contexto = (Frec, [(Evento, Frec)], [((Evento, Evento), Frec)])

a = [(60,1),(60,3),(50,2),(30,1),(65,3),(56,4),(67,1),(61,3),(55,4)]




obtFrecVacia :: Contexto -> Frec
obtFrecVacia (p, _, _) = p

procSecuencia :: [Evento] -> Contexto -> Contexto
procSecuencia [] c = c
procSecuencia l c = foldl agregarOrden1 (foldl agregarOrden0 c l) (creaPares l)

creaPares :: [a] -> [(a,a)]
creaPares [] = []
creaPares (x:[]) = []
creaPares (x:y:zs) = (x,y): creaPares (y:zs)


existeEven :: Evento -> Contexto -> Bool
existeEven even (_, p, _) = even `elem` map fst p

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
