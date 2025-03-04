import Data.Time.Clock --maneja tiempos y fechas
import Data.List -- proporciona funciones para listas
import System.IO --permite leer y escribir archivos
import Control.Exception -- maneja excepciones (errores)
import Control.DeepSeq (deepseq)-- fuerza al programa a evaluar TODO el contenido (externo e interno)
import System.Directory (doesFileExist) --verifica si un archivo existe

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    nombre :: String,
    carrera :: String,
    entrada :: UTCTime,
    edad :: Int,
    salida :: Maybe UTCTime
} deriving (Show, Read) -- show permite que los valores de tipo estudiante se conviertan en texto

-- Función para registrar la entrada de un estudiante a la universidad
registrarEntrada :: String -> String -> String -> Int -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada idEst nombre carrera edad tiempo universidad =
    Estudiante idEst nombre carrera tiempo edad Nothing : universidad

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida idEst tiempo universidad =
    map (\e -> if idEst == idEstudiante e then e { salida = Just tiempo } else e) universidad

-- Función para buscar un estudiante por su id
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante idEst universidad =
    find (\e -> idEst == idEstudiante e && isNothing (salida e)) universidad
    where isNothing Nothing = True
          isNothing _       = False

-- Función para calcular el tiempo que un estudiante permanece en la universidad
tiempoEnUniversidad :: Estudiante -> UTCTime -> (Int, Int, Int)
tiempoEnUniversidad estudiante tiempoActual =
    let tiempoTotal = case salida estudiante of
                        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada estudiante)
                        Nothing           -> diffUTCTime tiempoActual (entrada estudiante)
        totalSeconds = floor (realToFrac tiempoTotal :: Double)
        hours = totalSeconds `div` 3600
        minutes = (totalSeconds `mod` 3600) `div` 60
        seconds = totalSeconds `mod` 60
    in (hours, minutes, seconds)

-- Manejo de excepciones al guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad universidad = do
    resultado <- try (withFile "University.txt" WriteMode $ \h ->
        hPutStr h (unlines (map show universidad))) :: IO (Either IOException ())
    case resultado of
        Left ex -> putStrLn $ "Error al guardar el archivo: " ++ show ex
        Right _ -> putStrLn "Estudiantes guardados correctamente."

-- Manejo de excepciones al cargar la información de los estudiantes desde un archivo de texto
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    existe <- doesFileExist "University.txt"
    if not existe
        then return []
        else do
            contenido <- try (readFile "University.txt") :: IO (Either IOException String)
            case contenido of
                Left ex -> do
                    putStrLn $ "Error al leer el archivo: " ++ show ex
                    return []
                Right datos -> return (map read (lines datos))

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en la universidad."
listarEstudiantes estudiantes = mapM_ print estudiantes

-- Función principal del programa
main :: IO ()
main = do
    universidad <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de Universidad!"
    cicloPrincipal universidad

cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada del estudiante"
    putStrLn "2. Registrar salida del estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes en la universidad"
    putStrLn "5. Salir"
    
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            idEst <- getLine
            putStrLn "Ingrese el nombre del estudiante:"
            nombre <- getLine
            putStrLn "Ingrese la carrera del estudiante:"
            carrera <- getLine
            putStrLn "Ingrese la edad del estudiante:"
            edad <- readLn
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarEntrada idEst nombre carrera edad tiempoActual universidad
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada
        "2" -> do
            putStrLn "Ingrese el ID del estudiante al salir:"
            idEst <- getLine
            tiempoActual <- getCurrentTime
            let universidadActualizada = registrarSalida idEst tiempoActual universidad
            guardarUniversidad universidadActualizada
            cicloPrincipal universidadActualizada
        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            idEst <- getLine
            case buscarEstudiante idEst universidad of
                Just estudiante -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnUniversidad estudiante tiempoActual
                    putStrLn $ "El estudiante: " ++ nombre estudiante ++ " con identificacion: " ++  idEstudiante estudiante ++ " y edad: " ++ show (edad estudiante) ++" pertenciente al pregrado de: "++ carrera estudiante ++ " ha estado " ++ show tiempoTotal ++ " horas, minutos y segundos en la universidad. "
                Nothing -> putStrLn "Estudiante no encontrado."
            cicloPrincipal universidad
        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad
        "5" -> putStrLn "¡Hasta luego!"
        _   -> do
            putStrLn "Opción no válida, intente de nuevo."
            cicloPrincipal universidad
