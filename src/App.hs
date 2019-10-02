module App where


{-|
Comandos disponibles:
+ "preorden"
+ "inorden"
+ "postorden" : imprime el árbol en cualquiera de estos órdenes.
+ "buscar " ++ <indice>: busca el nodo especificado.
+ "insertar " ++ <indice> ++ <nombre>: trata de insertar un nodo en el árbol.
+ "eliminar" ++ <indice>: trata de eliminar el nodo especificado.
-}
app :: IO ()
app = do
  putStrLn "Bienvenido al gestor de árboles AVL. Qué desea hacer?"
  putStrLn "escriba 'comandos' para obtener una lista de comandos disponibles."
  command <- getLine
  putStr command
