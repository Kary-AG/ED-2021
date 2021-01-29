module Relaciones where
import Data.List

-- | reflexiveClojure. Función que calcula la cerradura reflexiva.
reflexiveClojure :: Eq b => [(b, b)] -> [(b, b)]
reflexiveClojure xs = nub (xs++ide(dom xs))

-- | simetricClojure. Función que calcula la cerradura simétrica
simetricClojure :: Eq b => [(b, b)] -> [(b, b)]
simetricClojure xs = nub(xs ++ inversa xs)

-- | transitiveClojure. Función que calcula la cerradura transitiva.
transitiveClojure :: Eq b => [(b, b)] -> [(b, b)]
transitiveClojure xs = transitive xs

-- | makeIrreflexive. Función que dada una relación, la vuelve irreflexiva.
makeIrreflexive :: Eq b => [(b, b)] -> [(b, b)]
makeIrreflexive xs = nub(xs\\(ide(dom xs)))

-- | isAntisimetric. Función que nos dice si una relación es antisimétrica.
isAntisimetric :: Eq b => [(b, b)] -> [(b, b)]
isAntisimetric      [] = []
isAntisimetric [(a,b)]=[(a,b)]
isAntisimetric  (x:xs) = if (snd x,fst x)`elem` xs then isAntisimetric (tail xs) else [x]++ isAntisimetric (tail xs)
                
-- | dom. Función auxiliar que calcula el Dominio de la relación.
dom :: [(a, b)] -> [a]
dom [] = []
dom [(a,b)] = [a]
dom (x:xs) = nub([fst x] ++ [snd x] ++ dom xs)

-- | ide. Función identidad
ide :: [b] -> [(b, b)]
ide [] =[]
ide (y:ys) =[(y,y)]++ ide ys

-- | inversa. Función inversa
inversa :: [(b, a)] -> [(a, b)]
inversa [] = []
inversa [(a,b)]=[(b,a)]
inversa (x:xs)= inversa [x] ++ inversa xs

-- | transitive. Función Auxiliar

transitive xs
    | conjunto xs = xs
    | otherwise    = transitive (xs `union` (parejas xs xs))
    
-- | conjunto. Función Auxiliar

conjunto r = subconjunto (parejas r r) r

-- | subconjutnto. Nos dice si un lista es sublista de otra
subconjunto :: (Eq a, Foldable t) => [a] -> t a -> Bool
subconjunto (x:xs) ys = [x | x<- xs, x `elem` ys] == xs

-- | parejas. Nos da los elementos faltantes en transitive.

parejas r s    = [(x,z) | (x,y) <- r, (y1,z) <- s, y == y1]


