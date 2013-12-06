type Evento = (Int, Int)
type Frec = Int --Frecuencia
type Contexto = (Frec, [(Evento, Frec)], [(Evento, Evento, Frec)])

obtFrecVacia :: Contexto -> Frec
obtFrecVacia (x, _, _) = x

existeEven :: Evento -> Contexto -> Bool
existeEven even (_, y, _) = even `elem` map fst y

agregarEven :: Evento -> Contexto -> Contexto
agregarEven even (0, [], []) = (1, [(even,1)], [])
agregarEven even (x, y, z) = (x+1, agregar' even y, z)
		where agregar' z ((a,b):xs)
			| z == a = (a,b+1):xs
			| otherwise = (a,b): agregar' z xs 
