module Compuertas where

-- | Compuerta. Clase que representa la abstraccion
-- de las compuertas aplicadas a cualquier tipo de datos
class Compuerta a where

  -- | id. Metodo que representa la identidad de un valor.
  (.!.) :: a -> a
  infixl 8 .!.

  -- | neg. Metodo que representa la compuerta NOT
  (.¬.) :: a -> a
  infixl 8 .¬.

  -- | conj, disy. Metodo que representa las compuertas AND, OR
  (.^.), (.|.) :: a -> a -> a
  infixl 7 .^., .|.

  -- | nand, nor. Metodo que representa la compuerta NAND, NOR
  (.*.), (.~.) :: a -> a -> a
  infixl 6 .*., .~.

  -- | xor. Metodo que representa la compuerta XOR
  (.+.) :: a -> a -> a
  infixl 5 .+.
