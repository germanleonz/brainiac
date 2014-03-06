{-|
    /Definicion de la cinta y las operaciones sobre ella/
-}
module Cinta (
    -- * Tipos exportados
    Cinta (..),
    -- * Funciones exportadas
    cintaVacia,
    conseguirPrimero,
    moverPrimero,
    modificarCasilla,
    extenderCinta
)
where

import qualified Data.Char as C
import qualified Data.Map  as DM

data Cinta = Cinta {
    primero :: Int,
    valores :: DM.Map Int Int,
    tamano  :: Int
} deriving (Eq)

instance Show Cinta where 
    show c = foldl (\cs v -> cs ++ [C.chr v]) [] (DM.elems $ valores c)

cintaVacia :: Int -> Cinta
cintaVacia n = Cinta 0 DM.empty n

conseguirPrimero :: Cinta -> Int 
conseguirPrimero c = DM.findWithDefault 0 (primero c) (valores c)

moverPrimero :: (Int -> Int) -> Cinta -> Cinta
moverPrimero fn c = c { primero = np }
    where 
        np = (fn $ primero c) `mod` t
        t  = tamano c

modificarCasilla :: (Int -> Int) -> Cinta -> Cinta
modificarCasilla fn c = c { valores = nuevosValores }
    where 
        pv            = conseguirPrimero c
        nuevosValores = DM.insert (primero c) (fn pv) (valores c)

extenderCinta :: Int -> Cinta -> Cinta
extenderCinta n c = c { tamano = nuevoTamano }
    where 
        nuevoTamano = (tamano c) + n
