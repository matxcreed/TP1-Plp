module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- recrExpr :: ... anotar el tipo ...
recrExpr (a->b)->(a->a->b)->(a->a->b->b->b)->(a->a->b->b->b)->(a->a->b->b->b)-> Expr -> b
recrExpr cnst rango suma resta mult div exp = case exp of 
                                              Const x   -> cnst x 
                                              Rango x y -> rango x y 
                                              Suma x y  -> suma x y  (rec x) (rec y)
                                              Resta x y -> resta x y (rec x) (rec y)
                                              Mult x  y -> mult x y  (rec x) (rec y)
                                              Div x  y  -> div  x y  (rec x) (rec y)
                                              where rec = recrExpr cnst rango suma resta mult div

-- foldExpr :: ... anotar el tipo ...
foldExpr (a->b)->(a->a->b)->(b->b->b)->(b->b->b)->(b->b->b)->(b->b->b)-> Expr -> b
foldExpr cnst rango suma resta mult div exp = case exp of 
                                              Const x   -> cnst x 
                                              Rango x y -> rango x y 
                                              Suma x y  -> suma  (rec x) (rec y)
                                              Resta x y -> resta (rec x) (rec y)
                                              Mult x  y -> mult  (rec x) (rec y)
                                              Div x  y  -> div   (rec x) (rec y)
                                              where rec = foldExpr cnst rango suma resta mult div



-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval  = foldr (id) (\r1 r2 -> conGenNormal (dameUno (r1, r2))) (\e1 e2 -> (fst e1 + fst e2,snd e1)) (\e1 e2 -> (fst e1 - fst e2,snd e1)) (\e1 e2 -> (fst e1 * fst e2,snd e1)) (/)

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = error "COMPLETAR EJERCICIO 9"

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
