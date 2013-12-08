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

obtprim :: Contexto -> Frec
obtprim (a,_,_) = a

obtseg :: Contexto -> [(Evento, Frec)]
obtseg (_,a,_) = a

obtter :: Contexto -> [((Evento,Evento), Frec)]
obtter (_,_,a) = a                 
                 
sumParFrec :: [(Frec,Frec)] -> Int
sumParFrec [] = 0
sumParFrec x = foldl (\a (b,c) -> a + (c - b)^2) 0 x

elimDups :: (Eq a) => [a] -> [a]
elimDups (x:xs) = if notElem x xs then
                      x:elimDups xs
                  else 
                      elimDups xs
elimDups [] = []

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