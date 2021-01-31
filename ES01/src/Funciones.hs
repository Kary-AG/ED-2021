{-
- Estructuras discretas 2021-1
- Profesor: Alma Rosario Arévalo Loyola
- Ayudante: José Ricardo Desales Santos
- Ayudante: Karla Socorro García Alcántara
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: Rodrigo Guadalupe Chávez Jiménez
- Practica 1: Introducción a Haskell y funciones.
- Integrantes:
-Azpeitia García Karyme Ivette
-Dorantes Perez Brando
-Valencia Cruz Jonathan Josué
-}

module Funciones where

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

type Numero = Int
type Proposicion = Bool
type  Area  = Double 
type Distancia = Double
type Parordenado = Double


-- | Función que regresa el sucesor de un número, esto es el número más uno.
sucN :: Numero -> Numero
sucN x = x + 1

-- | Función que regresa el máximo de dos números.
maxNum :: Numero -> Numero -> Numero
maxNum x y = if x >= y then x else y

-- | Función que suma dos números.
sumaNum :: Numero -> Numero -> Numero
sumaNum x y = x + y 

-- | Función que resta dos números (El primero menos el segundo).
restaNum :: Numero -> Numero -> Numero
restaNum x y = x - y

-- | Función que multiplica dos números.
multNum :: Numero -> Numero -> Numero
multNum x y = x*y

-- | Función que divide dos números (El primreo es el numerador).
divNum :: Numero -> Numero -> Numero
divNum x y = x`div`y

-- | Función que regresa la negación de una proposición.
--   Por ejemplo: True y False
negP :: Proposicion -> Proposicion
negP x = if x == True then False else True

-- | Función que regresa la conjunción de dos proposiciones. 
--   Por ejemplo: True y False
conjP :: Proposicion -> Proposicion -> Proposicion
conjP x y =  if x == True && y ==True then True else False  

-- | Función que regresa la disyunción de dos proposiciones.
--   Por ejemplo: True y False
disyP :: Proposicion -> Proposicion -> Proposicion
disyP x y = if x == True || y == True then True else False

-- | Función que calcula el valor absoluto de un número.
absNum :: Numero -> Numero
absNum x = if x>=0 then x  else x +((-2)*x)


-- | Función que regresa el área de un círculo.
areaCirc :: Area -> Area
areaCirc x = pi*(x**2)

-- | Función que regresa la distancia entre dos puntos (x1, y1), (x2. y2).
distancia :: Distancia-> Distancia -> Distancia -> Distancia -> Distancia
distancia x1 y1 x2 y2 = sqrt((x1-x2)^2+(y1-y2)^2)

-- | Función que calcula la suma de los primeros n números (Suma de Gauss).
sumaGauss :: Numero -> Numero
sumaGauss x  = ((x*(1+x))`div`2)


-- | Función que calcula el área de un triángulo dados tres puntos.
areaTri :: Parordenado -> Parordenado -> Parordenado-> Parordenado -> Parordenado -> Parordenado -> Parordenado
areaTri x1 y1 x2 y2 x3 y3  = (((x1*y2)+(x2*y3)+(x3*y1))-((x1*y3)+(x3*y2)+(x2*y1)))/ (2)

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------
pruebaEjemplo = sucN 5
-- Regresa: 6

maxNum1 = maxNum 1 0
-- Regresa: 1

maxNum2 = maxNum (-1) 0
-- Regresa: 0

sumaNum1 = sumaNum 1 3
-- Regresa: 4

sumaNum2 = sumaNum (-7) 8
-- Regresa: 1

restaNum1 = restaNum 9 6
-- Regresa: 3

restaNum2 = restaNum 1 3
-- Regresa: -2

restaNum3 = restaNum (-1) 1
-- Regresa: -2

multNum1 = multNum 0 3
-- Regresa: 0

multNum2 = multNum 9 8
-- Regresa: 72

divNum1 = divNum 4 2
-- Regresa: 2

divNum2 = divNum 9 4
-- Regresa: 2

negP1 = negP True
-- Regresa: False

negP2 = negP False
-- Regresa: True

conjP1 = conjP True True
-- Regresa: True

conjP2 = conjP False True
-- Regresa: False

disyP1 = disyP False False
-- Regresa: False

disyP2 = disyP True False
-- Regresa: True

absNum1 = absNum 9
-- Regresa: 9

absNum2 = absNum (-9)
-- Regresa: 9

areaCirc1 = areaCirc 2
-- Regresa: 12.57

areaCirc2 = areaCirc 2.5
-- Regresa: 19.63

distancia1 = distancia 1 2 (-3) 4
-- Regresa: 4.47

distancia2 = distancia (-3) 0 (-4) 6
-- Regresa: 6.08

sumaGauss1 = sumaGauss 10
-- Regresa: 55

sumaGauss2 = sumaGauss 1000
-- Regresa: 500500

areaTri1 = areaTri (-8) (-2) 4 6 1 5
-- Regresa: 6.0

areaTri2 = areaTri (-8) (-2) 4 6 (-1) (-5)-- Regresa: -46.0
