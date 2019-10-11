module Funciones where

import           Tipos

-- función de conveniencia para obtener la altura de un árbol.
altura :: ArbolAVL a b -> Int
altura Nulo                     = 0
altura (NodoAVL _ _ altura _ _) = altura

left Nulo                  = Nulo
left (NodoAVL _ _ _ izq _) = izq
right Nulo                   = Nulo
right (NodoAVL _ _ _ _ drch) = drch

-- obtiene el factor de balance del árbol.
balance :: ArbolAVL a b -> Int
balance Nulo                     = 0
balance (NodoAVL _ _ _ izq drch) = altura drch - altura izq

{-|
busca un elemento en el árbol.
si el árbol está vacío, el objeto no está.
si el índice de la raíz del árbol es el buscado, se retorna el nodo.
si es menor, se busca en el sub-árbol derecho.
si es mayor, se busca en el izquierdo.
el tipo del índice debe ser de clase Ord, que se pueda ordenar.
-}
buscar :: (Ord indice)
       => indice -> ArbolAVL indice datos -> Maybe datos
buscar _ Nulo = Nothing
buscar buscado (NodoAVL indice datos alt izq drch)
  | indice == buscado = Just datos
  | indice < buscado = buscar buscado drch
  | indice > buscado = buscar buscado izq

{-
imprime el árbol de tres formas diferentes.
-}
preorden :: (Show indice, Show datos)
         => ArbolAVL indice datos -> String
preorden Nulo = ""
preorden (NodoAVL indice datos _ izq drch) =
  "(" ++ show indice ++ ": " ++ show datos ++ preorden izq ++ preorden drch ++ ")"

inorden :: (Show indice, Show datos)
        => ArbolAVL indice datos -> String
inorden Nulo = ""
inorden (NodoAVL indice datos _ izq drch) =
  inorden izq ++ show indice ++ ": " ++ show datos ++ inorden drch

postorden :: (Show indice, Show datos)
          => ArbolAVL indice datos -> String
postorden Nulo = ""
postorden (NodoAVL indice datos _ izq drch) =
  postorden izq ++ postorden drch ++ show indice ++ ": " ++ show datos

{-|
elimina un nodo del árbol.
Si el árbol está vacío, se retorna igual.
Si el índice a eliminar es mayor al actual, se elimina en el subárbol derecho.
Si el índice a eliminar es menor al actual, se elimina en el subárbol izquierdo.
Si el índice a eliminar es igual al actual:
- Si el nodo actual es una hoja, se retorna un árbol nulo.
- Si el nodo actual tiene una sola rama, se retorna el subárbol respectivo.
- Si el nodo actual tiene dos ramas, se retorna el elemento más grande de la
  rama izquierda, con hijos la rama derecha y la rama izquierda, a la que se
  ha eliminado este elemento.
-}
eliminar :: (Ord indice)
         => indice -> ArbolAVL indice datos -> ArbolAVL indice datos
eliminar _ Nulo = Nulo
eliminar ind (NodoAVL indice datos alt izq drch)
  | ind > indice = let nuevoDrch = eliminar ind drch
                   in balancear (NodoAVL indice datos
                                 (1 + max (altura nuevoDrch) (altura izq))
                                 izq nuevoDrch)
  | ind < indice = let nuevoIzq = eliminar ind izq
                   in balancear (NodoAVL indice datos
                                 (1 + max (altura drch) (altura nuevoIzq))
                                 nuevoIzq drch)
  | ind == indice = case (izq, drch) of
      (Nulo, Nulo) -> Nulo
      (Nulo, drch) -> drch
      (izq, Nulo)  -> izq
      (izq, drch)  -> (NodoAVL id de
                       (1 + max (altura izq) (altura drch))
                       (eliminar id izq) drch)
  where
    masGrande (NodoAVL i d a z Nulo) = (NodoAVL i d a z Nulo)
    masGrande (NodoAVL i d a z dc)   = masGrande dc
    (NodoAVL id de _ _ _) = masGrande izq


{-|
balancea un árbol, si es necesario.
Si el árbol está vacío, se retorna igual.
Si el árbol es un nodo con hijos:
- si el factor de balance se calcula entre -1 y 1, se retorna tal y como está.
- si el factor de balance es menor a -1:
  - si el factor de balance del hijo izquierdo es menor a -1, se realiza una
    rotación izquierda.
  - si el factor de balance del hijo derecho es mayor a 1,
-}
balancear :: ArbolAVL a b -> ArbolAVL a b
balancear Nulo = Nulo
balancear (NodoAVL indice datos alt izq drch)
  | (altura drch - altura izq) `elem` [-1,0,1]
  = (NodoAVL indice datos alt izq drch)
  | (altura drch - altura izq) < -1
  = case balance izq of
      -1 -> rotarDrc $ NodoAVL indice datos alt izq drch
      1  -> rotarDrc $ NodoAVL indice datos
        (1 + max (altura (rotarIzq izq)) (altura drch)) (rotarIzq izq) drch
      _  -> error "imposible"
  | (altura drch - altura izq) > 1
  = case balance drch of
      -1 -> rotarIzq $ NodoAVL indice datos
        (1 + max (altura izq) (altura (rotarDrc drch))) izq (rotarDrc drch)
      1  -> rotarIzq $ NodoAVL indice datos alt izq drch
      _  -> error "imposible"

rotarIzq :: ArbolAVL a b -> ArbolAVL a b
rotarIzq Nulo = Nulo
rotarIzq (NodoAVL _ _ _ _ Nulo) =
  error "rotacion izquierda sobre arbol sin hijo derecho"
rotarIzq (NodoAVL ind0 dat0 alt0 izq0 (NodoAVL ind1 dat1 alt1 izq1 drch1)) =
  (NodoAVL ind1 dat1 alt1' (NodoAVL ind0 dat0 alt0' izq0 izq1) drch1)
  where
    alt0' = 1 + max (altura izq0) (altura izq1)
    alt1' = 1 + max alt0' (altura drch1)

rotarDrc :: ArbolAVL a b -> ArbolAVL a b
rotarDrc Nulo = Nulo
rotarDrc (NodoAVL _ _ _ Nulo _) =
  error "rotación derecha sobre árbol sin hijo izquierdo"
rotarDrc (NodoAVL ind0 dat0 alt0 (NodoAVL ind1 dat1 alt1 izq1 drch1) drch0) =
  (NodoAVL ind1 dat1 alt1' izq1 (NodoAVL ind0 dat0 alt0' drch1 drch0))
  where
    alt0' = 1 + max (altura drch0) (altura drch1)
    alt1' = 1 + max alt0' (altura izq1)

{-|
inserta un elemento en el árbol.
Si el árbol está vacío, se retorna el nuevo nodo raíz (con ambos hijos nulos).
Si el árbol es un nodo con hijos:
- si el índice del nodo es el mismo del insertado, se retorna el árbol sin
  modificar.
- si es menor, se inserta en la rama derecha, y se balancea el árbol.
- si es mayor, se inserta en la izquierda, y se balancea el árbol.
-}
insertar :: (Ord indice)
         => indice -> datos -> ArbolAVL indice datos -> ArbolAVL indice datos
insertar insertado datosIn Nulo = NodoAVL insertado datosIn 1 Nulo Nulo
insertar insertado datosIn (NodoAVL indice datos alt izq drch)
  | indice == insertado = (NodoAVL indice datos alt izq drch)
  | indice < insertado = let nuevoDrch = insertar insertado datosIn drch
                         in balancear $ (NodoAVL indice datos
                                         (1 + max (altura izq) (altura nuevoDrch))
                                         izq nuevoDrch)
  | indice > insertado = let nuevoIzq = insertar insertado datosIn izq
                         in balancear $ (NodoAVL indice datos
                                         (1 + max (altura nuevoIzq) (altura drch))
                                         nuevoIzq drch)

{-|
actualiza un elemento del árbol.
Si el árbol está vacío, se retorna como está.
Si el árbol es un nodo con hijos:
- si el índice del nodo es el mismo del insertado, se retorna el árbol con los
  datos modificados.
- si es menor, se actualiza la rama derecha.
- si es mayor, se actualiza la rama izquierda.
-}
actualizar :: (Ord indice)
           => indice -> datos -> ArbolAVL indice datos -> ArbolAVL indice datos
actualizar actualizado datosIn Nulo = Nulo
actualizar actualizado datosIn (NodoAVL indice datos alt izq drch)
  | indice == actualizado = (NodoAVL indice datosIn alt izq drch)
  | indice < actualizado = let nuevoDrch = actualizar actualizado datosIn drch
                           in (NodoAVL indice datos alt izq nuevoDrch)
  | indice > actualizado = let nuevoIzq = actualizar actualizado datosIn izq
                           in (NodoAVL indice datos alt nuevoIzq drch)
