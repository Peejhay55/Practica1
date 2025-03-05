import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)
import System.IO.Error (isDoesNotExistError, isPermissionError)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    matricula :: String,
    ingreso :: UTCTime,
    egreso :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la institución o ya salió
} deriving (Show, Read)

-- Función para registrar el ingreso de un estudiante
registrarIngreso :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarIngreso matriculaEstudiante tiempo institucion =
    Estudiante matriculaEstudiante tiempo Nothing : institucion

-- Función para registrar el egreso de un estudiante
registrarEgreso :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEgreso matriculaEstudiante tiempo institucion =
    map (\e -> if matriculaEstudiante == matricula e then e { egreso = Just tiempo } else e) institucion

-- Función para buscar un estudiante por su matrícula
encontrarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
encontrarEstudiante matriculaEstudiante institucion =
    find (\e -> matriculaEstudiante == matricula e && isNothing (egreso e)) institucion
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante permaneció en la institución
tiempoEnInstitucion :: Estudiante -> UTCTime -> NominalDiffTime
tiempoEnInstitucion estudiante tiempoActual =
    case egreso estudiante of
        Just tiempoEgreso -> diffUTCTime tiempoEgreso (ingreso estudiante)
        Nothing           -> diffUTCTime tiempoActual (ingreso estudiante)

-- Función para guardar la información de los estudiantes en un archivo
guardarInstitucion :: String -> IO ()
guardarInstitucion contenido = do
    resultado <- try (withFile "institucion.txt" AppendMode (\handle -> hPutStrLn handle contenido)) :: IO (Either IOError ())
    case resultado of
        Left ex -> putStrLn $ "Error guardando la institución: " ++ show ex
        Right _ -> putStrLn "Datos guardados correctamente."

-- Función para reintentar una operación en caso de error
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left _ -> do
            threadDelay 1000000
            reintentar (n - 1) accion
        Right val -> return (Right val)

-- Función para cargar la información de los estudiantes desde un archivo
cargarInstitucion :: IO [Estudiante]
cargarInstitucion = do
    resultado <- try (readFile "institucion.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            if isDoesNotExistError ex
                then do
                    putStrLn "El archivo `institucion.txt` no existe. Creando un archivo nuevo."
                    writeFile "institucion.txt" ""
                    return []
                else if isPermissionError ex
                    then do
                        putStrLn "Error de permisos al acceder al archivo `institucion.txt`."
                        return []
                    else do
                        putStrLn $ "Error cargando la institución: " ++ show ex
                        return []
        Right contenido ->
            return (mapMaybe parsearEstudiante (lines contenido))
    where
        parsearEstudiante :: String -> Maybe Estudiante
        parsearEstudiante linea =
            case words linea of
                [matricula, ingreso, egreso] -> Just $ Estudiante matricula (read ingreso) (readMaybeEgreso egreso)
                _ -> Nothing

        readMaybeEgreso :: String -> Maybe UTCTime
        readMaybeEgreso "Nothing" = Nothing
        readMaybeEgreso egresoStr = Just (read egresoStr)

-- Función para leer la información de los estudiantes desde un archivo
leerInstitucion :: IO [Estudiante]
leerInstitucion = cargarInstitucion

-- Función para listar los estudiantes actualmente en la institución
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes institucion = do
    let estudiantesActivos = filter (\e -> egreso e == Nothing) institucion
    if null estudiantesActivos
        then putStrLn "No hay estudiantes en la institución."
        else mapM_ (\e -> putStrLn $ "Matrícula: " ++ matricula e ++ ", Ingreso: " ++ show (ingreso e)) estudiantesActivos

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante estudiante =
    matricula estudiante ++ "," ++ show (ingreso estudiante) ++ "," ++ show (egreso estudiante)

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal institucion = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar ingreso de estudiante"
    putStrLn "2. Registrar egreso de estudiante"
    putStrLn "3. Buscar estudiante por matrícula"
    putStrLn "4. Listar los estudiantes en la institución"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la matrícula del estudiante:"
            matriculaEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let institucionActualizada = registrarIngreso matriculaEstudiante tiempoActual institucion
            putStrLn $ "Estudiante con matrícula " ++ matriculaEstudiante ++ " registrado."
            guardarInstitucion (unlines (map mostrarEstudiante institucionActualizada))
            cicloPrincipal institucionActualizada

        "2" -> do
            putStrLn "Ingrese la matrícula del estudiante a salir:"
            matriculaEstudiante <- getLine
            tiempoActual <- getCurrentTime
            let institucionActualizada = registrarEgreso matriculaEstudiante tiempoActual institucion
            putStrLn $ "Estudiante con matrícula " ++ matriculaEstudiante ++ " egresó."
            guardarInstitucion (unlines (map mostrarEstudiante institucionActualizada))
            cicloPrincipal institucionActualizada

        "3" -> do
            putStrLn "Ingrese la matrícula del estudiante a buscar:"
            matriculaEstudiante <- getLine
            case encontrarEstudiante matriculaEstudiante institucion of
                Just estudiante -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnInstitucion estudiante tiempoActual
                    putStrLn $ "El estudiante con matrícula " ++ matriculaEstudiante ++ " está en la institución."
                    putStrLn $ "Tiempo en institución: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la institución."
            cicloPrincipal institucion

        "4" -> do
            listarEstudiantes institucion
            cicloPrincipal institucion

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal institucion

 -- Función principal del programa
main :: IO ()
main = do
    institucion <- cargarInstitucion
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes!"
    cicloPrincipal institucion