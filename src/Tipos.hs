module Tipos where

-- enumeración de los posibles comandos que se le pueden dar al programa.
data Command indice datos = Preorden | Inorden | Postorden | Comandos | Salir
  | Buscar indice           -- búsqueda con el índice.
  | Insertar indice datos   -- insertar un elemento con los datos.
  | Actualizar indice datos -- actualizar un elemento con los datos.
  | Eliminar indice         -- eliminar un elemento.

-- estructura que contiene los datos de la persona.
data Persona = Persona
  { nombrePersona :: String }

-- instancia especial de Show para Persona. (sólo muestra el nombre)
instance Show Persona where
  show persona = nombrePersona persona

-- definición del árbol binario como estructura de datos abstracta.
data ArbolAVL indice datos = Nulo
  | NodoAVL
    indice                   -- el índice del nodo. debe ser instancia de Ord
    datos                    -- la tupla con los datos.
    Int                      -- la altura del árbol.
    (ArbolAVL indice datos)  -- la rama izquierda
    (ArbolAVL indice datos)  -- la rama derecha

-- instanciación del árbol para los valores requeridos.
type Arbol = ArbolAVL Int Persona
