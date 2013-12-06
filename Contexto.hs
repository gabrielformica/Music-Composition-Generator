type Evento = (Int, Int)
type Frec = Int --Frecuencia
type Contexto = (Frec, [(Evento, Frec)], [((Evento, Evento), Frec)])

obtFrecVacia :: Contexto -> Frec
obtFrecVacia (x, _, _) = x

existeEven :: Evento -> Contexto -> Bool
existeEven even (_, y, _) = even `elem` map fst y

agregarOrden0 :: Evento -> Contexto -> Contexto
agregarOrden0 a (0, [], []) = (1, [(a, 1)], [])
agregarOrden0 a (x, y, z) = (x+1, agregar a y, z)

agregarOrden1 :: (Evento, Evento) -> Contexto -> Contexto
agregarOrden1 a (x, y, z) = (x, y, agregar a z)

agregar :: (Eq a) => a -> [(a, Frec)] -> [(a, Frec)]
agregar x [] = [(x,1)]
agregar x ((b, c):ys) =
				if x == b then
					(b, c+1):ys
				else
					(b,c): agregar x ys
