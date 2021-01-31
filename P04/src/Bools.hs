{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Ejercicio Semanal 2: Nuestros Booleanos
- Integrantes:
-
-
-}

module Bools
  ((.^.),
   (.|.),
   (.¬.),
   (.->.),
   (.<->.),
   transform,
   MyBool (..)
  ) where

import Compuertas

data MyBool = BTrue | BFalse deriving ( Eq, Ord)


instance Compuerta MyBool where

  (.!.) a = a
  
  (.¬.) BTrue = BFalse
  (.¬.) BFalse = BTrue

  (.^.) BFalse b = BFalse
  (.^.) a BFalse = BFalse
  (.^.) BTrue BTrue = BTrue 

  (.|.) a BTrue = BTrue
  (.|.) BTrue b = BTrue
  (.|.) BFalse BFalse = BFalse

  -- xor
  p .+. q
    | p == q = BFalse
    | otherwise = BFalse

  -- nand
  p .*. q
    | p == q = BFalse
    | otherwise = BTrue

  -- nor
  p .~. q
    | p == q = (.¬.)p
    | otherwise = BFalse

(.->.) :: MyBool -> MyBool -> MyBool --ex
infixr 2 .->.
(.->.) p q = (.|.) q ((.¬.)p)

(.<->.) :: MyBool -> MyBool -> MyBool --ex
infixr 2 .<->.
(.<->.)p q = (.^.)((.->.) p q)((.->.) p q)

instance Show MyBool where
  show BTrue = "#t"
  show BFalse = "#f"

-- | transform. Función que transforma el tipo Bool de haskell a
-- nuestros booleanos.
transform :: Bool -> MyBool
transform True  = BTrue
transform False = BFalse
