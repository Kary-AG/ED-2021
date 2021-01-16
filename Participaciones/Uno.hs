
module BTree where
  
import Data.List
data BTree a = Void | Node a (BTree a) (BTree a) deriving(Eq)
instance Show a => Show (BTree a) where
  show t = showSubL t ""

--------------------------------------------------------------------------------
--                                 PRETTY PRINT                               --
--------------------------------------------------------------------------------

-- | showSubL. Función que hace un 'prettyPrint' de los árboles. │
showSubL (Node n v1@Void v2@Void) s = show n ++
                                      nS ++ "├──(L) V" ++
                                      nS ++ "└──(R) V"
  where nS = "\n" ++ s
showSubL (Node n Void r) s          = show n ++
                                      nS ++ "├──(L) V" ++
                                      nS ++ "└──(R) " ++ showSubL r oS
  where
    nS = "\n" ++ s
    oS = addWhite 12 s
showSubL (Node n l Void) s          = show n ++
                                      nS ++ "├──(L) " ++ showSubL l oS ++ 
                                      nS ++ "└──(R) V"
  where
    nS = "\n" ++ s
    oS = addWhite 12 s
showSubL (Node n l r) s             = show n ++
                                      nS ++ "├──(L) " ++ showSubL l oS ++
                                      nS ++ "└──(R) " ++ showSubL r oS
  where
    nS = "\n" ++ s
    oS = addWhite 12 s
-- Auxiliar
addWhite 0 s = s
addWhite n s = if (n < 0)
               then s
               else addWhite (n-3) s ++ " "

-- Árbol de prueba 1
test :: BTree Int
test = (Node 1
         (Node 2
           (Node 9
             (Void)
             (Node 10 (Void) (Void))
           )
           (Node 4
             (Node 5 (Void) (Void))
             (Node 6 (Void) (Void))
           )
         )
         (Node 3
           (Node 7 (Void) (Void))
           (Node 8 (Void) (Void))
         )
       )
-- Árbol de prueba 2
test2 :: BTree Int
test2 = (Node 1
          (Node 2
            (Void)
            (Node 4
              (Node 5 (Void) (Void))
              (Node 6 (Void) (Void))
            )
          )
          (Node 3
            (Node 7 (Void) (Void))
            (Node 8 (Void) (Void))
          )
        )

-- | mapTree. Función que pueda cambiar un árbol(Un tipo map) pero para nuestros BTree.

mapTree :: (a->b)  -> BTree a -> BTree b 
mapTree a  Void = Void
mapTree a (Node e t1 t2) = Node (a e) (mapTree a t1)  (mapTree a t2)

--------------------------------------------------------------------------------
--                                 ZONA COVID                                 --
--------------------------------------------------------------------------------
type COVID = (String, Bool)

propagacion :: BTree COVID -> BTree COVID
propagacion Void = Void
propagacion (Node (a,b) t1 t2)
  | b == False = Node (a,b) (propagacion t1) (propagacion t2)
  |otherwise = Void

-- | Fúnciones Auxiliares

empty :: BTree COVID
empty = Node ("a", True ) Void Void

nul :: BTree COVID
nul = Node ("a’", False ) Void Void

left :: BTree COVID
left = Node ("a", False ) ( empty ) ( Node ("c", False ) nul nul )

-- Árbol de prueba 3
test3 ::  BTree COVID
test3 = Node (" Rodrigo" , False )( Node ("Karlita" , True ) Void Void ) ( Node ( " Desales" , False )(Node ("Emi",True)Void Void ) Void)


--------------------------------------------------------------------------------
--                                 ¿Qué hace la siguiente Función?            --
--------------------------------------------------------------------------------

f = 1 : 1 : zipWith (+) f ( tail f )
{-
La función nos regresa la sucesión de Fibonacci usando recursión pero dado que zipWith ocupa los términos de f,solo se evaluara  
cuando sea necesarios es decir si hacemos take 2 f, Haskell no necesita evaluarlo dado que ya se conocen los 2 términos de [1,1],
creo que esto pasa por ser lazy evaluation < D: >

take 2
f                        = 1 : 1 : < D:>
tail f                   = 1: < D: >
zipWith (+) f ( tail f ) = < D: >

Con take 3
f                        = 1: 1 : 2 : < D: >
tail f                   = 1 : 2 : < D: >
zipWith (+) f ( tail f ) =  2 :< D: >

Y así sucesivamente se va evaluando si Haskell no conoce los términos, pero deja de evaluar cuando llegue al término que necesita. 
-}
