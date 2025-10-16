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
import GHC.Arr (listArray)

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
recrExpr :: (Float->b)->(Float->Float->b)->
            (Expr->Expr->b->b->b)->
            (Expr->Expr->b->b->b)->
            (Expr->Expr->b->b->b)->
            (Expr->Expr->b->b->b)-> Expr -> b
recrExpr cnst rango suma resta mult div exp = case exp of
                                              Const x   -> cnst x
                                              Rango x y -> rango x y
                                              Suma x y  -> suma x y  (rec x) (rec y)
                                              Resta x y -> resta x y (rec x) (rec y)
                                              Mult x  y -> mult x y  (rec x) (rec y)
                                              Div x  y  -> div  x y  (rec x) (rec y)
                                              where rec = recrExpr cnst rango suma resta mult div

-- foldExpr :: ... anotar el tipo ...
foldExpr :: (Float->b)->(Float->Float->b)->
            (b->b->b)->
            (b->b->b)->
            (b->b->b)->
            (b->b->b)-> Expr -> b
foldExpr cnst rango suma resta mult div = recrExpr cnst rango (\_ _ rx ry -> suma rx ry) 
                                                              (\_ _ rx ry -> resta rx ry) 
                                                              (\_ _ rx ry -> mult rx ry) 
                                                              (\_ _ rx ry -> div rx ry)
--foldExpr cnst rango suma resta mult div exp = case exp of
--                                              Const x   -> cnst x
--                                              Rango x y -> rango x y
--                                              Suma x y  -> suma  (rec x) (rec y)
--                                              Resta x y -> resta (rec x) (rec y)
--                                              Mult x  y -> mult  (rec x) (rec y)
--                                              Div x  y  -> div   (rec x) (rec y)
--                                              where rec = foldExpr cnst rango suma resta mult div


-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval  = foldExpr (\x gen -> (x, gen))
                 (\r1 r2 -> dameUno (r1, r2))
                 (operacion (+))
                 (operacion (-))
                 (operacion (*))
                 (operacion (/))
      where operacion f ex ey gen = let 
                                      (x, genx) = ex gen
                                      (y, geny) = ey genx
                                    in 
                                      (f x y, geny)
-- >>> fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0))   
-- 5.92308

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f = \gen -> (histograma m (rango95 (fst (genLista gen))) (fst (genLista gen)), (snd (genLista gen)))
                          where genLista = muestra f n

-- >>> armarHistograma 5 10 (dameUno (1.0, 20.0)) (genNormalConSemilla 1)
-- (Histograma 4.459915 3.2997673 [0,1,3,2,3,1,0],<Gen>)

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 100.03403 1.0675964 [0,0,3,0,0,1,1,2,0,2,0,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 100.99388 0.81878525 [241,289,507,825,1080,1350,1426,1333,1117,754,539,287,252],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = recrExpr show (\x y -> show x ++ "~" ++ show y)
                        (\e1 e2 r1 r2 -> maybeParen (parEnSuma e1) r1 ++ " + " ++ maybeParen (parEnSuma e2) r2)
                        (\e1 e2 r1 r2 -> maybeParen (parEnResta e1) r1 ++ " - " ++ maybeParen (parEnResta e2) r2)
                        (\e1 e2 r1 r2 -> maybeParen (parEnMult e1) r1 ++ " * " ++ maybeParen (parEnMult e2) r2)
                        (\e1 e2 r1 r2 -> maybeParen (parEnDiv e1) r1 ++ " / " ++ maybeParen (parEnDiv e2) r2)

          where parEnSuma e = constructor e == CEResta || constructor e == CEMult || constructor e == CEDiv
                parEnResta e = constructor e == CEResta || constructor e == CESuma || constructor e == CEMult || constructor e == CEDiv
                parEnMult e = constructor e == CEResta || constructor e == CESuma || constructor e == CEDiv
                parEnDiv e = constructor e == CEResta || constructor e == CEMult || constructor e == CESuma

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
