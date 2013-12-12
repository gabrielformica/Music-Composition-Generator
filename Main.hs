module Main where

import Prelude hiding (init)
import Input
import Euterpea hiding (Event)
import Data.List
import Data.Function
import Contexto

main :: IO ()
main = componer

-- Directorio predeterminado
directorio :: String
directorio = "./xml/"

-- Longitud de las secuencias musicales generadas
longitud :: Int
longitud = 50

{- Induce un modelo de contexto a partir de la colección musical 
   en el directorio por defecto, genera una secuencia musical 
   nueva a partir de este modelo, la imprime por pantalla y la 
   reproduce.
   -}
componer :: IO ()
componer = componer' directorio

componer' :: String -> IO ()
componer' dir = do
  (seqs, filenames) <- loadMusicXmls dir
  let modelo = foldl procSecuencia (0,[],[]) seqs 
  let composicion = obtComp (-1,-1) modelo longitud
  putStrLn $ show composicion
  --play $ sequenceToMusic composicion
  --putStrLn $ show modelo

{- Recupera las diez secuencias más similares a la k-ésima secuencia 
   de la colección musical en el directorio por defecto, donde la 
   colección musical ha sido ordenada en orden alfabético por el 
   nombre de archivo. Imprime una lista ordenada de las diez 
   secuencias más similares. En cada fila de la lista se debe indicar 
   el número de la secuencia (relativo al orden alfabético de la 
   colección), el nombre de archivo y la distancia a la consulta.
   -}
buscar :: Int -> IO ()
buscar = buscar' directorio
  
buscar' :: String -> Int -> IO ()
buscar' dir n = do
  seqfns <- loadMusicXmls dir
  let (seqs, filenames) = unzip $ sortBy (compare `on` snd) $ (uncurry zip) seqfns
  let lista = listify ([1..(length seqs)],seqs,filenames)
  if (n > 0) && (n <= length seqs) then
      mapM_ (\(a,b,c) -> putStrLn (show a ++ "\t" ++ b ++ "\t" ++ show c)) $ tail $ take 10 $ sortBy (compare `on` obtter2) (calcDistList lista (seqs !! (n-1)))
    else
      putStrLn "Indice fuera de rango"

tocar :: Int -> IO ()
tocar n = do
  seqfns <- loadMusicXmls directorio
  let (seqs, filenames) = unzip $ sortBy (compare `on` snd) $ (uncurry zip) seqfns
  if (n > 0) && (n <= length seqs) then
    putStrLn (filenames !! (n-1)) >>
    play (sequenceToMusic (seqs !! (n-1)))
    else
      putStrLn "Indice fuera de rango"
          
eventToNote :: Evento -> Music Note1
eventToNote e = note
  where
  d = (fromIntegral $ snd e) / 16
  p = Euterpea.pitch $ fst e
  note = Prim (Note d (p,[]))
  
sequenceToMusic :: [Evento] -> Music Note1
sequenceToMusic es = line $ map eventToNote es

