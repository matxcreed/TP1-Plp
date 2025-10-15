-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
--vacio :: Int -> (Float, Float) -> Histograma
--vacio n (l, u) = Histograma l ((u - l)/ fromIntegral n) [0|_ <- [0..(n+1)]]

---- correccion-----
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l ((u-l) / fromIntegral n) (replicate (n+2) 0)


----ej 4
--agregar :: Float -> Histograma -> Histograma
--agregar x (Histograma i t cs) = Histograma i t (actualizarElem pos (+1) cs)
--                                where lista = listaDeIntervalos (Histograma i t cs)
--                                      pos = ubicarPos x lista
--
--
listaDeIntervalos:: Histograma-> [(Float,Float)]
listaDeIntervalos (Histograma i t cs) = [ if j==0 then (infinitoNegativo,i)
                                          else if j==len then(i+t*fromIntegral (j-1),infinitoPositivo)
                                          else (i+t*fromIntegral (j-1),i+t*(fromIntegral (j-1)+1))| j <- [0..len]]
                                      where len = length cs - 1
--
--ubicarPos:: Float -> [(Float,Float)] -> Int
--ubicarPos x = foldl (\ac (j,k) -> if j<=x then ac+1 else ac ) (-1)


----correccion-----

agregar :: Float -> Histograma -> Histograma
agregar n (Histograma i t cs) = if n < i then Histograma i t (actualizarElem 0 (+1) cs) else 
                                Histograma i t (actualizarElem (pos ) (+1) cs)
                                 where pos = min (1+floor((n-i)/t)) (length cs - 1)

  --- uso n-i para calcular cuánto se pasa mi n del inicio del intervalo. si el intervalo inicia en 20, con amplitud de 2
  --- y mi n es 25 eso quiere decir que me "pasé" 5 unidades del primer casillero,
  --- para saber en qué casillero caigo, lo divido por t (la amplitud de cada casillero. y me da 2,5.Esto quiere decir que me
  --- pasé ** 2,5 veces un casillero ** del casillero incial (el de + inf) a esto le aplico floor
  ---y obtengo que debí caer en el 2 casillero (o sea el tercero, ya que ignoramos el primero por tratarlo en la primera parte del if))
  



-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n interv = foldl (\ac f -> agregar f ac) (vacio n interv)

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros (Histograma i t cs) = zipWith (\n (j,k) -> Casillero j k n (if total == 0 then 0 else (fromIntegral n/total)*100.0)) cs intervalos
                            where total = fromIntegral (sum cs)
                                  intervalos = listaDeIntervalos (Histograma i t cs)
