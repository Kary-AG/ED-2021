{-
- Estructuras Discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Ing. Rodrigo Guadalupe Chávez Jiménez
- Practica 4: Interpretaciones de la lógica.
- Integrantes:
- Azpeitia García Karyme Ivette
- Dorantes Perez Brando
- Valencia Cruz Jonathan Josué
-}

module Formulas where

import Bools
import Data.List

data Cuant = Forall Name Cuant | Exists Name Cuant
  | PP Pred
  | NegP Cuant
  | ConjP Cuant Cuant
  | DisyP Cuant Cuant
  | ImplP Cuant Cuant
  | SyssP Cuant Cuant deriving (Eq)

data Pred = PrU Un Uni | PrB Bi Bin | PrT Tr Tri deriving (Eq)

data Un = A | B | C | D | E | F | G | H | I deriving (Show, Eq)

data Bi = J | K | L | M | N | O | P | Q | R deriving (Show, Eq)

data Tr = S | T | U | V | W | X | Y | Z deriving (Show, Eq)

data Prop =
  Var String
  | Neg Prop
  | Conj Prop Prop
  | Disy Prop Prop
  | Impl Prop Prop
  | Syss Prop Prop deriving(Show)

-- Todas las variables que se encuentren aquí, supondremos que son
-- verdaderas.
type Modelo = [String]

-- Variables
type Name = String

-- Alias para las tupas con aridad n.
type Uni = (Name)
type Bin  = (Name, Name)
type Tri = (Name, Name, Name)

instance Show Cuant where
  show (Forall x body) = "∀" ++ show x ++ " (" ++ show body ++ " )"
  show (Exists x body) = "∃" ++ show x ++ " (" ++ show body ++ " )"
  show (NegP body)     = "¬(" ++ show body ++ ")"
  show (ConjP e1 e2)   = "( " ++ show e1 ++ " ∧ " ++ show e2 ++ " )"
  show (DisyP e1 e2)   = "( " ++ show e1 ++ " ∨ " ++ show e2 ++ " )"
  show (ImplP e1 e2)   = "( " ++ show e1 ++ " → " ++ show e2 ++ " )"
  show (SyssP e1 e2)   = "( " ++ show e1 ++ " ↔ " ++ show e2 ++ " )"
  show (PP pred)       = show pred

instance Show Pred where
  show (PrU b a) = show b ++ show a
  show (PrB b a) = show b ++ show a
  show (PrT b a) = show b ++ show a

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | fv. Función que regresa las variables libres de una expresión
-- (Cuant)
fv :: Cuant -> [Name]
fv (Forall name body) = fv body\\[name]
fv (Exists name body) = fv body\\[name]
fv (ConjP e1 e2)      = fv e1 `union` fv e2
fv (DisyP e1 e2)      = fv e1 `union` fv e2
fv (ImplP e1 e2)      = fv e1 `union` fv e2
fv (SyssP e1 e2)      = fv e1 `union` fv e2
fv (NegP e1)          = fv e1
fv (PP p)             = fvP p

-- | fvP. Función auxiliar que regresa las variables de un
-- predicado. Te puedes dar cuenta que dado el constructor de aridad n
-- (n <- [1,2,3]) no importa el predicado, solo los argumentos de
-- este.
fvP :: Pred -> [Name]
fvP (PrU _ a) = [a]
fvP (PrB _ (a,b)) =[a,b] 
fvP (PrT _ (a,b,c)) = [a,b,c]

-- | lv. Función que regresa las variables ligadas de una expresión
-- (Cuant)
lv :: Cuant -> [Name]
lv (Forall name body) = [name] ++ lv body
lv (Exists name body) = [name] ++ lv body
lv (ConjP e1 e2)      = lv e1  ++ lv e2
lv (DisyP e1 e2)      = lv e1  ++ lv e2
lv (ImplP e1 e2)      = lv e1  ++ lv e2
lv (SyssP e1 e2)      = lv e1  ++ lv e2
lv (NegP e1)          = lv e1
lv (PP p)             = []

-- | niegalo. Función que niega una proposición.
niegalo :: Prop -> Prop
niegalo (Var p) = Neg(Var p)
niegalo (Conj (p) q)= Disy (niegalo p) (niegalo q)
niegalo (Disy p q)= Conj (niegalo p)(niegalo q)
niegalo (Impl p q )= Conj (p) (Neg q)
niegalo (Syss p q) = Disy (Conj p (Neg q))(Conj q (Neg p))

-- | quitaImps. Función que quita las implicaciones de una
-- proposición, regresando una expresión equivalente.
quitaImps :: Prop -> Prop
quitaImps (Var x)= (Var x)
quitaImps (Impl p q)= (Disy (Neg p)(q))
quitaImps (Neg (p)) = (Neg (quitaImps (p)))
quitaImps (Conj (p) (q))= (Conj (quitaImps (p))(quitaImps (q)))
quitaImps (Disy (p) (q))= (Disy (quitaImps (p))(quitaImps (q)))
quitaImps (Syss p q)= (Conj(quitaImps (Impl (p)(q))) (quitaImps(Impl(q)(p))))

-- | interpreta. Función que interpreta una proposición dados sus
-- valores de verdad.
interpreta :: Modelo -> Prop -> MyBool
interpreta [] (Var x)   = BFalse
interpreta xs (Var x)   = transform $ elem x xs 
interpreta xs (Neg x)   = if (interpreta xs x) == BTrue then BFalse else BTrue
interpreta xs (Conj p q)= if (interpreta xs p) == BTrue && (interpreta xs q)==BTrue then BTrue else BFalse
interpreta xs (Disy p q)= if (interpreta xs p) == BTrue || (interpreta xs q)==BTrue then BTrue else BFalse
interpreta xs (Impl p q)= if (interpreta xs p) == BTrue && (interpreta xs q)==BFalse then BFalse else BTrue
interpreta xs (Syss p q)
  | (interpreta xs p) == BTrue && (interpreta xs q)==BTrue = BTrue
  | (interpreta xs p) == BFalse && (interpreta xs q)==BFalse = BTrue
  | otherwise = BFalse
  

{--
 --	B: Es un bebé.
 --	M: Puede manejar un cocodrilo.
 --	L: Es lógico.
 -- 	D: Es despistado.
 --
 --}

-- | e1. Función que representa el enunciado: Todos los bebé son ilógicos.
e1 :: Prop
e1 = (Impl (Var "B")(Neg (Var "L")))

-- | e2. Función que representa el enunciado: Nadie que sea despistado puede manejar un cocodrilo.
e2 :: Prop
e2 = (Impl (Var "D")(Neg (Var "M")))

-- | e3. Función que representa el enunciado: Las personas ilógicas son despistadas.
e3 :: Prop
e3 = (Impl(Neg (Var"L"))(Neg (Var "D")))

-- | e. Función que representa el enunciado: e1 /\ e2 /\ e3 -> (B -> ¬M) 
e :: Prop
e = (Impl(Conj (Conj(e1)(e2))(e3)) (Impl(Var"B") (Neg (Var "M"))))

-- | modelo. Función que representa el modelo con el que e es satisfacible.
modelo :: Modelo
modelo = ["M","L"]

--------------------------------------------------------------------------------
--------                             EJEMPLOS                           --------
--------------------------------------------------------------------------------

niegalo1 = niegalo (Var "P")
-- Regresa: Neg (Var "P")

niegalo2 = niegalo (Impl (Var "P") (Var "Q"))
-- Regresa: Conj (Var "P") (Neg (Var "Q"))

quitaImps1 = quitaImps (Conj (Impl (Var "P") (Var "Q")) (Var "P"))
-- Regresa: Conj (Disy (Neg (Var "P")) (Var "Q")) (Var "P")

quitaImps2 = quitaImps (Syss (Var "P") (Var "Q"))
-- Regresa: Conj (Disy (Neg (Var "P")) (Var "Q")) (Disy (Neg (Var "Q")) (Var "P"))

interpreta1 = interpreta [] (Disy (Var "P") (Neg (Var "P")))
-- Regresa: #t

interpreta2 = interpreta ["Q"] (Conj (Var "Q") (Neg (Var "Q")))
-- Regresa: #f

interpreta3 = interpreta ["M", "L"] e
-- Regresa: #t

-- Nuevos ejemplos sobre expresiones con lógica de predicados.
forall1 = Forall "x" (Exists "y" (PP (PrB P ("x", "y"))))
-- P(x,y) -> x es primo de y
-- Para todo 'x' Existe 'y' tal que P('x', 'y')

forall2 = Forall "a" (ImplP (PP (PrU A ("a"))) (Exists "b" (PP (PrB K ("a", "b")))))
-- A(x)   -> x es azul
-- K(j,k) -> k es creado a partir de j
-- Para todo 'a' si 'a' es azul entonces existe 'b' tal que K('a', 'b')

exists1 = Exists "una persona" (PP (PrT Z ("una persona", "Phil", "Pookie")))
-- Z(a,b,c) -> a es sobrino de b y de c
-- Existe 'una persona' tal que 'una persona' es zobrino de 'Phil' y 'Pookie'

exists2 = Exists "oveja" (Exists "muchacho" (SyssP
                                             (PP (PrB Q("muchacho","oveja")))
                                             (Exists "herramienta" (PP (PrT T("muchacho", "oveja", "herramienta"))))))
-- Q(a,b)   -> a qüida a b (a cuida a b)
-- T(x,y,z) -> x trasquila a y con z
-- existe una oveja existe un muchacho tal que el muchacho qüida
-- (cuida) a la oveja syss existe una herramienta tal que el muchacho
-- trasquila a la oveja con la herramienta

-- Ejemplos sobre lf, fv
fv1 = fv (Forall "x" (NegP (PP (PrU A ("z")))))
-- Regresa: ["z"]

fv2 = fv (Exists "y" (ConjP (PP (PrU A ("z"))) (PP (PrB P ("y", "x")))))
-- Regresa: ["z", "x"]

lv1 = lv (Forall "x" (NegP (PP (PrU A ("x")))))
-- Regresa: ["x"]

lv2 = lv (Exists "y" (ConjP (PP (PrU A ("z"))) (PP (PrB P ("y", "x")))))
-- Regresa: ["y"]

lv3 = lv (ConjP (PP (PrU A ("z"))) (PP (PrB P ("y", "x"))))
-- Regresa: []
