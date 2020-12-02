{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Practica 5: Nuestras estructuras. listas
- Integrantes:
- Azpeitia García Karyme Ivette
- Dorantes Perez Brando
- Valencia Cruz Jonathan Josué
-}

module Lists where

data List a = Void | Cons a (List a)
instance (Show a)=> Show (List a)where 
  show (Void)  = "[]"
  show (Cons a x)  = "(" ++ show a   ++ ":" ++ show x ++ ")" 

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | cabeza. Función que regresa tal vez la cabeza de la lista.
cabeza :: List a -> Maybe a
cabeza         Void = Nothing 
cabeza (Cons a (x)) = Just a

-- | cola. Función que regresa tal vez la cola de la lista.
cola :: List a -> Maybe (List a)
cola        Void = Nothing
cola (Cons a (x))= Just x

-- | ultimo. Función que regresa tal vez el último elemento de la
-- lista.
ultimo :: List a -> Maybe a
ultimo          Void = Nothing
ultimo (Cons a Void) = Just a
ultimo  (Cons a (x)) = ultimo x

-- | longitud. Función que regresa la longitud de la lista.
longitud :: List a -> Int
longitud         Void = 0
longitud (Cons a (x)) = 1 + longitud x 

-- | existe. Función que nos dice si un elemento está en una lista.
existe :: (Eq a) => List a -> a -> Bool
existe Void  x = False
existe (Cons a (x)) b
  | a == b = True
  | otherwise = existe x b 

-- | concatL. Función que concatena por la izquierda (l ++ [x]).
concatL :: List a -> a -> List a
concatL         Void a = (Cons a (Void))
concatL (Cons a (x)) b = (Cons a (concatL x b))

-- | concatR. Función que concatena por la derecha ([x] ++ l).
concatR :: a -> List a -> List a
concatR       a Void = (Cons a Void)
concatR b (Cons a x) = (Cons b  (Cons a (x))) 

-- | reversa. Función que regresa la reversa de una lista.
reversa :: List a -> List a
reversa           Void = Void
reversa (Cons a Void ) = concatL  Void  a
reversa     (Cons a x) = concatL (reversa x) a

-- | concatcito. Función que concatena dos listas.
concatcito :: List a -> List a -> List a
concatcito Void Void = Void
concatcito (Cons a x) Void       = (Cons a x)
concatcito  Void (Cons a x)      = (Cons a x)
concatcito (Cons a x) (Cons a' x') = concatcito (concatL (Cons a x) a') x' 


-- | toHaskell. Función que pasa una de nuestras listas a las listas
-- de haskell.
toHaskell :: List a -> [a]
toHaskell Void = []
toHaskell (Cons a x) = [a] ++ toHaskell x

-- | fromHaskell. Función que pasa una lista de haskell a nuestras
-- listas.
fromHaskell :: [a] -> List a
fromHaskell     [] = Void
fromHaskell    [a] = (Cons a Void)
fromHaskell (a:as) = (Cons a (fromHaskell as))

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------

-- Lista que contiene a los primeros cinco elementos.
l1 = (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Void)))))

-- Lista que contiene a los elementos del 6-10.
l2 = (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Void)))))

cabeza1 = cabeza l1
-- Regresa: Just 1

cabeza2 = cabeza Void
-- Regresa: Nothing

cola1 = cola l1
-- Regresa: Just (2:(3:(4:(5:[]))))

cola2 = cola Void
-- Regresa: Nothing

ultimo1 = ultimo l2
-- Regresa: Just 10

ultimo2 = ultimo Void
-- Regresa: Nothing

longitud1 = longitud l1
-- Regresa: 5

longitud2 = longitud l1
-- Regresa: 5

longitud3 = longitud Void
-- Regresa: 0

existe1 = existe l1 9
-- Regresa: False

existe2 = existe l2 9
-- Regresa: True

concatL1 = concatL l1 6
-- Regresa: (1:(2:(3:(4:(5:(6:[]))))))

concatL2 = concatL l2 11
-- Regresa: (6:(7:(8:(9:(10:(11:[]))))))

concatR1 = concatR 0 l1
-- Regresa: (0:(1:(2:(3:(4:(5:[]))))))

concatR2 = concatR 5 l2
-- Regresa: (5:(6:(7:(8:(9:(10:[]))))))

reversa1 = reversa l1
-- Regresa: (5:(4:(3:(2:(1:[])))))

reversa2 = reversa l2
-- Regresa: (10:(9:(8:(7:(6:[])))))

concatcito1 = concatcito l1 l2
-- Regresa: (1:(2:(3:(4:(5:(6:(7:(8:(9:(10:[]))))))))))

concatcito2 = concatcito l2 l1
-- Regresa: (6:(7:(8:(9:(10:(1:(2:(3:(4:(5:[]))))))))))

toHaskell1 = toHaskell l1
-- Regresa: [1,2,3,4,5]

toHaskell2 = toHaskell l2
-- Regresa: [6,7,8,9,10]

fromHaskell1 = fromHaskell [1,2,3]
-- Regresa: (1:(2:(3:[])))

fromHaskell2 = fromHaskell []
-- Regresa: Void
