{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Ejercicio Semanal 2: Nuestros Booleanos
- Integrantes:
-Azpeitia García Karyme Ivette
-
-}

module Bools where

import Compuertas

data MyBool = BTrue | BFalse deriving (Eq, Ord) 


instance Compuerta MyBool where

  -- | Método que representa la identidad para el tipo MyBool
  (.!.) a = a

  -- | Método que representa la compuerta neg para el tipo MyBool.
  (.¬.) BTrue  = BFalse
  (.¬.) BFalse = BTrue

  -- | Método que representa la compuerta and para el tipo MyBool.
  (.^.)  BFalse b = BFalse
  (.^.)  a BFalse = BFalse
  (.^.)  BTrue BTrue = BTrue 

  -- | Método que representa la compuerta or para el tipo MyBool.
  (.|.) a  BTrue  = BTrue
  (.|.) BTrue  b = BTrue
  (.|.) BFalse BFalse = BFalse

  -- | Método que representa la compuerta xor para el tipo MyBool.
  a .+. b 
    | a == b = BTrue
    | otherwise = BFalse
  -- | Método que representa la compuerta nand para el tipo MyBool.
  a .*. b  
    | a == b = BFalse
    | otherwise = BTrue  
  -- | Método que representa la compuerta nor para el tipo MyBool.
  a .~. b  
   | a /= b = BFalse
   | a == b = (.¬.) a

-- | Operador que representa la implicación. ## --ex
(.->.) :: MyBool -> MyBool -> MyBool 

(.->.) a b =  (.|.) b ((.¬.) a)

-- | Operador que representa la doble implicación. ## --ex
(.<->.) :: MyBool -> MyBool -> MyBool 
(.<->.) a b = (.^.)  ((.->.) a b) ((.->.) b a)

instance Show MyBool where
  show BTrue  = "#t"
  show BFalse = "#f"