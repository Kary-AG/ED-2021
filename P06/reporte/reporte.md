---
title: "Estructuras Discretas"
subtitle: "Práctica 06: Nuestras EStructuras Pt. 2"
author: | 
  | "Karyme I. Azpeitia García"
  | "Dorantes Perez Brando"
  | "Valencia Cruz Jonathan Josué"
date: "12/11/2020"
output: pdf_document
---
1. Sea $F$ la función que toma un natural $n$ y devuelve la lista de naturales entre $1$ y $n. Es decir,
$$
F(n) = [1, 2, 3, ..., n]
$$

(a) Defina recursivamente a $F$.
```{Haskell}
F :: (Eq a, Num a) => a -> [a]                                                  
F 1 = [1]                                                                       
F n = f (n-1) ++ [n]
```
(b) Demuestre que ```fac(n) = prodl (F n)```, donde fac es la función factorial y ```prodl``` es la función que toma una lista de naturales y devuelve el producto de sus elementos.

Demostración por Inducción sobre $n$, considerando $\mathbb{N}-\{0\}$

**Caso Base** Probamos para $n=1$.

Es fácil verificar, pues por como estan definidas las funciones ```fac 1```, ```prodl [x]```,```F 1```, tenemos
```{Haskell}
fac 1 = 1 = prodl [1] = prodl F 1
```

**Hipótesis Inductiva**

Supongamos que se cumple para $n\in\mathbb{N}$, esto quiere decir 
```{Haskell}
fac n = prodl F n
```

**Paso Inductivo**

Queremos ver que se cumple para el sucesor de $n$.

Pd. ```fac (n+1) = prodl F(n+1)```

Procedemos saliendo del lado derecho de la igualdad

```{Haskell}
prodl F(n+1) =
             = prodl (F (n+1)-1 ++ [n+1]) --Por definición recursiva de F.
             = prodl (F n ++ [n+1]) --Por aritmetica (*).
```

*Obs. 1* Por la firma de ```F``` sabemos que  ```F n``` es de tipo ```xs``` por lo que  ```F n ++ [n+1]``` es de la forma ```(x:xs)```.

Continuando con (*)
```{Haskell}
            = prodl F n * prodl [n+1] --Por Obs.1 y defición recursiva de prodl.
            = fac n * (n+1) --Por hipótesis inductiva.
                            --y definición recursiva de prodl.
            = fac n+1 --Por definición recursiva  de fac.
```

Así que por el principio de inducción, concluimos que se cumple para cualquier $n\in\mathbb{N}$.

2. Sea $G$ la función que toma dos naturales $k$ y $n$ y devuelve la lista con $n$ apariciones del número $k$. Es decir,
$$
G(k, n) = [k1, k2, ..., kn]
$$

(a) Defina recursivamente a $G$.

```{Haskell}
G :: (Eq p, Num p) => p -> p -> [p]                                             
G k 1 = [k]                                                                     
G k n = G k (n-1) ++ [k]
```

(b) Demuestre que ```sum (replica k n) = k * n```` donde sum es la función que toma una lista de naturales y devuelve su suma.

Demostración por inducción sobre $n$ considerando $\mathbb{N}-\{0\}$

**Caso Base**  Probamos que se cumple para $n=1$.

Es fácil verificar, pues por como estan definidas las funciones ```replica k 1```, ```sum [x] = x```, ``` k * 1```, tenemos
```{Haskell}
sum(replica k 1)= sum [k] = k = k *1
```

**Hipótesis Inductiva**

Supongamos que se cumple para $n\in\mathbb{N}$, esto quiere decir 
```{Haskell}
sum(replica k n) = k * n
```

**Paso Inductivo**

Queremos ver que se cumple para el sucesor de $n$.

Pd. ```sum(replica k n+1) = k * n + 1```

Procedemos partiendo del lado izquierdo de la igualdad

```{Haskell}
sum(replica k n+1) =
                   = sum(replica k  ((n+1)-1) ++ [n+1]) --Por definición recursiva de replica.
                   = sum (replica k  n) + k --Por definición recursiva de sum.
                   =  k*n + k --Por hipótesis de inducción.
                   =  k * n+1 --Por definición recursiva de (*).
```

Así que por el principio de inducción, concluimos que se cumple para cualquier $n\in\mathbb{N}$.
