module Compuertas where

-- | Compuerta. Clase que representa la abstraccion
-- de las compuertas aplicadas a cualquier tipo de datos
class Compuerta a where

  -- | id. Metodo que representa la identidad de un valor.
  (.!.) :: a -> a

  -- | neg. Metodo que representa la compuerta NOT
  (.¬.) :: a -> a

  -- | conj, disy. Metodo que representa las compuertas AND, OR
  (.^.), (.|.) :: a -> a -> a

  -- | nand, nor. Metodo que representa la compuerta NAND, NOR
  (.*.), (.~.) :: a -> a -> a

  -- | xor. Metodo que representa la compuerta XOR
  (.+.) :: a -> a -> a
