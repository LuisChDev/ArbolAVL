module App where

import           Data.IORef
import           Data.List                    (foldl')
import           Funciones
import           System.Exit
import           Text.ParserCombinators.ReadP
import           Text.Read                    (readMaybe)
import           Tipos

-- lee el comando del usuario.
parseComando :: ReadP String
parseComando = do
  comando <- many1 $ satisfy $ \char -> char >= 'a' && char <= 'z'
  -- satisfy (== ' ')
  return comando

-- lee un índice.
parseIndice :: (Read indice) => ReadP indice
parseIndice = do
  ind <- many1 $ satisfy $ \char -> char /= ' ' && char /= '\t'
  case readMaybe ind of
    Nothing   -> pfail
    Just indx -> return indx

-- lee el contenido de un valor.
-- los datos se deben ingresar en el formato (x,y)
parsePersona :: ReadP Persona
parsePersona = do
  nombre <- many1 $ satisfy $ \char ->
    (char >= 'A' && char <= 'Z') ||
    (char >= 'a' && char <= 'z')
  return Persona {nombrePersona = nombre}

-- Parser completo.
parseInput :: (Read indice)
           => String -> [(Command indice Persona, String)]
parseInput = readP_to_S $ do
  comando <- parseComando
  case comando of
    "comandos"   -> return Comandos
    "salir"      -> return Salir
    "preorden"   -> return Preorden
    "inorden"    -> return Inorden
    "postorden"  -> return Postorden
    "buscar"     -> do
      satisfy (== ' ')
      indice <- parseIndice
      return $ Buscar indice
    "insertar"   -> do
      satisfy (== ' ')
      indice <- parseIndice
      satisfy (== ' ')
      persona <- parsePersona
      return $ Insertar indice persona
    "actualizar" -> do
      satisfy (== ' ')
      indice <- parseIndice
      satisfy (== ' ')
      persona <- parsePersona
      return $ Actualizar indice persona
    "eliminar"   -> do
      satisfy (== ' ')
      indice <- parseIndice
      return $ Eliminar indice
    _            -> pfail

{-|
Lee e interpreta el valor de entrada del usuario como un comando.
-}
askCommand :: (Read indice) => IO (Command indice Persona)
askCommand = do
  input <- getLine
  case parseInput input of
    [] -> do
      putStrLn "Entrada inválida."
      putStrLn "escriba 'comandos' para obtener una lista de comandos disponibles."
      askCommand
    lista -> return $ fst $ last lista

{-|
Comandos disponibles:
+ "comandos"
+ "preorden"
+ "inorden"
+ "postorden" : imprime el árbol en cualquiera de estos órdenes.
+ "buscar " ++ <indice>: busca el nodo especificado.
+ "insertar " ++ <indice> ++ <nombre>: trata de insertar un nodo en el árbol.
+ "actualizar" ++ <indice> ++ <nombre> : trata de actualizar en el lugar.
+ "eliminar" ++ <indice>: trata de eliminar el nodo especificado.
+ "salir" : termina la ejecución del programa.
-}

app :: IORef (ArbolAVL Int Persona) -> IO ()
app arbol = do
  putStrLn "\nIngrese su comando.\n"
  usrComm <- askCommand
  case usrComm of
    Comandos -> do
      putStrLn "Comandos disponibles:"
      putStrLn "\"comandos\""
      putStrLn "\"preorden\""
      putStrLn "\"inorden\""
      putStrLn "\"postorden\" : imprime el árbol en cualquiera de estos órdenes."
      putStrLn "\"buscar <indice>\": busca el nodo especificado."
      putStrLn $ "\"insertar <indice> <nombre>\": trata de insertar un nodo en"
        ++ "el árbol."
      putStrLn $ "\"actualizar <indice> <nombre>\": trata de actualizar en"
        ++ " el lugar."
      putStrLn "\"eliminar <indice>\": trata de eliminar el nodo especificado."
      putStrLn "\"salir\" : termina la ejecución del programa."
      app arbol

    Salir -> do
      putStrLn "saliendo del programa."
      exitSuccess

    Preorden -> do
      curArbol <- readIORef arbol
      putStrLn $ preorden curArbol
      app arbol

    Inorden -> do
      curArbol <- readIORef arbol
      putStrLn $ inorden curArbol
      app arbol

    Postorden -> do
      curArbol <- readIORef arbol
      putStrLn $ postorden curArbol
      app arbol

    Buscar indice -> do
      curArbol <- readIORef arbol
      case (buscar indice curArbol) of
        Nothing ->
          putStrLn "Lo sentimos, este elemento no está en el árbol" >> app arbol
        Just persona -> (putStrLn $ "Esta cédula identifica a la persona "
          ++ (nombrePersona persona)) >> app arbol

    Insertar indice datos -> do
      curArbol <- readIORef arbol
      case buscar indice curArbol of
        Just _ -> do
          putStrLn "Al parecer esta persona ya está en el árbol"
          app arbol
        Nothing -> do
          writeIORef arbol (insertar indice datos curArbol)
          putStrLn "inserción exitosa"
          app arbol

    Actualizar indice datos -> do
      curArbol <- readIORef arbol
      case buscar indice curArbol of
        Nothing -> do
          putStrLn "Este individuo no está en el árbol"
          app arbol
        Just _ -> do
          writeIORef arbol (actualizar indice datos curArbol)
          putStrLn "actualización exitosa"
          app arbol

    Eliminar indice -> do
      curArbol <- readIORef arbol
      case buscar indice curArbol of
        Nothing -> do
          putStrLn "Este individuo no está en el árbol"
          app arbol
        Just _ -> do
          writeIORef arbol (eliminar indice curArbol)
          putStrLn "eliminación exitosa"
          app arbol


ejecutable :: IO ()
ejecutable = do
  putStrLn "Bienvenido al gestor de árboles AVL. Qué desea hacer?"
  arbol <- newIORef Nulo
  app arbol
