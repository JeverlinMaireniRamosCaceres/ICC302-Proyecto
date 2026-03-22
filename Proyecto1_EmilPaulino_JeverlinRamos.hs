
-- Proyecto 2
-- Emil Paulino, ID: 10154329
-- Jeverlin Ramos, ID: 10154300


-- INTERPRETE DE LENGUAJE FUNCIONAL (EVALUADOR DE EXPRESIONES ARITMETICAS)
-- El evaluador de expresiones aritmeticas calcula el resultado de la expresion construida usando tipos algebraicos
-- utilizando recursion, manejo de errores con Either, y separacion de logica e IO


-- Evaluacion de errores
-- Tipo definido para poder representar los errores posibles durante la evaluacion de una expresion
data EvalError = DivisionPorCero | OperacionInvalida | VariableNoDefinida String
instance Show EvalError where
    show DivisionPorCero = "Error. Division por cero"
    show (VariableNoDefinida var) = "Error. La variable " ++ var ++ " no esta definida"
    show OperacionInvalida = "Error. Operacion invalida"

-- Tipos algebraicos
-- Definicion del tipo algebraico propio, solo es posible suma, resta, multiplicacion y division

-- Se define este tipo algebraico con las operaciones basicas aritmeticas de suma, resta, multiplicacion y division. Esto con el proposito de realizar las operaciones
-- y poder utilizar pattern matching

data TipoAlgebraico = Suma | Resta | Multiplicacion | Division 

-- La funcion aplicarTipo recibe un operador (que es TipoAlgebraico), y dos numeros a, y devuelve otro numero si todo es ejecutado correctamente
-- a: es un tipo de dato general, que representa que puede ser cualquier tipo de numero
-- Se utiliza Fractional a, Eq a para permitir operar con distintos tipos de numeros y que la division y la comparacion con cero sea correcta

aplicarTipo :: (Fractional a, Eq a) => TipoAlgebraico -> a -> a -> Either EvalError a

-- Definicion de las funciones con pattern matching

-- Suma
aplicarTipo Suma x y = Right (x + y)

-- Resta 
aplicarTipo Resta x y = Right (x - y)

-- Multiplicacion
aplicarTipo Multiplicacion x y = Right (x * y)

-- Division por cero
aplicarTipo Division _ 0 = Left DivisionPorCero

-- Division normal
aplicarTipo Division x y = Right (x / y)

-- Definir expresion recursiva

-- Tipo de dato que representa una expresion aritmetica, un valor o una operacion aplicada a dos expresiones
-- La expresion aritmetica a evaluar puede ser un valor sencillo o una operacion que se aplique a dos expresiones,
-- lo que permite construir expresiones complejas a partir de expresiones pequeñas
data Expr a = Valor a | Var String |Aplicar TipoAlgebraico (Expr a) (Expr a)

-- Funcion que evalua de forma recursiva una expresion y maneja errores usando Either
evalua :: (Fractional a, Eq a) => Expr a -> Either EvalError a

-- Evalua de forma recursiva una expresion y retorna Either EvalError a
-- Si ambas subexpresiones evaluan correctamente (Right), se aplica el operador
-- Si alguna de estas expresiones falla, se propaga el error con Left
-- El caso Var retorna OperacionInvalida porque evalua no tiene acceso al entorno

evalua (Valor n) = Right n
evalua (Var _) = Left OperacionInvalida
evalua (Aplicar op exp1 exp2) =
  combinar (evalua exp1) (evalua exp2)
  where
    combinar (Right valor1) (Right valor2) = aplicarTipo op valor1 valor2
    combinar (Left err) _ = Left err
    combinar _ (Left err) = Left err

-- Recolectar variables
-- Recorre el arbol de forma recursiva y recolecta los nombres de todas las variables usadas
-- Es una funcion pura, no depende de IO ni del entorno, solo del arbol de expresiones
-- Caso base - Valor: un numero no tiene variables, retorna lista vacia
-- Caso base - Var: encontro una variable, retorna su nombre en una lista
-- Caso recursivo - Aplicar: recorre ambos lados del arbol y une los resultados
recolectarVariables :: Expr a -> [String]
recolectarVariables (Valor _) = []
recolectarVariables (Var nombre) = [nombre]
recolectarVariables (Aplicar _ exp1 exp2) = recolectarVariables exp1 ++ recolectarVariables exp2

-- Entorno de variables
type Env = [(String, Double)]

-- Buscar variable en el entorno
-- buscarVar recibe el nombre de una variable y un entorno, y devuelve el valor asociado o un error
-- Se usa recursion explicita sobre la lista, si la lista esta vacia, la variable no existe (error)
-- Si el primer par coincide con el nombre buscado, se retorna su valor con Right
-- Si no coincide, se sigue buscando en el resto de la lista
buscarVar :: String -> Env -> Either EvalError Double
buscarVar nombre [] = Left (VariableNoDefinida nombre)
buscarVar nombre ((var, val):resto)
    | nombre == var = Right val
    | otherwise = buscarVar nombre resto

-- Instance Show para Expr
-- Permite convertir una expresion a texto de forma legible para el usuario
-- Cada caso del tipo algebraico Expr tiene su propia representacion: valores se muestran directamente,
-- variables con su nombre, y operaciones con parentesis para mayor claridad
instance Show a => Show (Expr a) where
    show (Valor n) = show n
    show (Var nombre) = nombre
    show (Aplicar op e1 e2) = "(" ++ show e1 ++ " " ++ showOp op ++ " " ++ show e2 ++ ")"

-- Funcion auxiliar que muestra el simbolo del operador
showOp :: TipoAlgebraico -> String
showOp Suma = "+"
showOp Resta = "-"
showOp Multiplicacion = "*"
showOp Division = "/"

-- Evaluacion con entorno
-- Recibe un entorno y una expresion, y devuelve el resultado o un error, gracias al uso de Either
-- Funciona de forma recursiva siguiendo la estructura del arbol de expresiones
-- - Valor: retorna el numero directamente
-- - Var: busca la variable en el entorno usando buscarVar
-- - Aplicar: evalua ambas subexpresiones y, si ambas son exitosas, aplica el operador
evaluar :: Env -> Expr Double -> Either EvalError Double
evaluar _   (Valor n) = Right n
evaluar env (Var nombre) = buscarVar nombre env
evaluar env (Aplicar op e1 e2) =
    case evaluar env e1 of
        Left err     -> Left err
        Right valor1 ->
            case evaluar env e2 of
                Left err     -> Left err
                Right valor2 -> aplicarTipo op valor1 valor2

-- Evaluar multiples expresiones usando fold
-- evaluarTodas aplica evaluar a una lista de expresiones y retorna una lista de resultados
-- foldr recorre la lista de derecha a izquierda, acumulando resultados
-- Cada elemento se evalua independientemente, por lo que un error en una expresion
-- no afecta la evaluacion de las demas, cada resultado es un Either separado
evaluarTodas :: Env -> [Expr Double] -> [Either EvalError Double]
evaluarTodas env = foldr (\ expr acc -> evaluar env expr : acc) []

-- Profundidad del arbol de expresiones
-- profundidad es una funcion pura y recursiva que calcula la altura del arbol de expresiones
-- Caso base Valor y Var: son hojas del arbol, profundidad 0
-- Caso recursivo Aplicar: la profundidad es 1 mas el maximo entre las profundidades de ambos subarboles
profundidad :: Expr a -> Int
profundidad (Valor _) = 0
profundidad (Var _) = 0
profundidad (Aplicar _ e1 e2) = 1 + max (profundidad e1) (profundidad e2)

-- Procesar resultado usando composicion de funciones
-- procesarResultado convierte un Either EvalError Double en un mensaje legible para el usuario
-- Se implementa usando composicion de funciones con (.) para encadenar dos funciones
-- formatear: convierte el Either a String segun si fue exito o error
-- agregarEtiqueta: agrega una pequeña descripcion al mensaje
formatear :: Either EvalError Double -> String
formatear (Left err)  = "Error: " ++ show err
formatear (Right val) = "Resultado: " ++ show val

agregarEtiqueta :: String -> String
agregarEtiqueta mensaje = "[Evaluacion] " ++ mensaje

procesarResultado :: Either EvalError Double -> String
procesarResultado = agregarEtiqueta . formatear

-- Interaccion con el usuario
-- Separa la logica y el IO

-- Pide al usuario que defina una variable y la agrega al entorno
definirVariable :: Env -> IO Env
definirVariable env = do
    putStrLn "Nombre de la variable:"
    nombre <- getLine
    putStrLn "Valor de la variable:"
    valStr <- getLine
    case (reads valStr :: [(Double, String)]) of
        [(val, "")] -> do
            putStrLn $ "Variable " ++ nombre ++ " = " ++ show val ++ " definida"
            return ((nombre, val) : env)
        _ -> do
            putStrLn "Valor invalido, ingrese un numero valido"
            definirVariable env

-- Pide al usuario que elija un operando simple (numero o variable)
pedirOperando :: Env -> IO (Expr Double)
pedirOperando env = do
    putStrLn "  1. Numero"
    putStrLn $ "  2. Variable (disponibles: " ++ show (map fst env) ++ ")"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "  Ingrese el numero:"
            nStr <- getLine
            case (reads nStr :: [(Double, String)]) of
                [(n, "")] -> return (Valor n)
                _         -> do
                    putStrLn "  Numero invalido, ingrese un numero valido"
                    pedirOperando env
        "2" -> do
            putStrLn "  Ingrese el nombre de la variable:"
            nombre <- getLine
            return (Var nombre)
        _ -> do
            putStrLn "  Opcion invalida, ingrese 1 o 2"
            pedirOperando env

-- Solicita el operando derecho al usuario
pedirOperandoDerecho :: Env -> IO (Expr Double)
pedirOperandoDerecho env = do
    putStrLn "  1. Numero"
    putStrLn $ "  2. Variable (disponibles: " ++ show (map fst env) ++ ")"
    putStrLn "  3. Otra operacion (anidar)"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "  Ingrese el numero:"
            nStr <- getLine
            case (reads nStr :: [(Double, String)]) of
                [(n, "")] -> return (Valor n)
                _         -> do
                    putStrLn "  Numero invalido, ingrese un numero valido."
                    pedirOperandoDerecho env
        "2" -> do
            putStrLn "  Ingrese el nombre de la variable:"
            nombre <- getLine
            return (Var nombre)
        "3" -> construirExpr env
        _ -> do
            putStrLn "  Opcion invalida, ingrese 1, 2 o 3"
            pedirOperandoDerecho env

construirExpr :: Env -> IO (Expr Double)
construirExpr env = do
    putStrLn ""
    putStrLn "Primer valor:"
    izq <- pedirOperando env
    putStrLn "Operacion:"
    op  <- pedirOperacion
    putStrLn "Segundo valor:"
    der <- pedirOperandoDerecho env
    return (Aplicar op izq der)

-- Pide al usuario que elija una operacion aritmetica
pedirOperacion :: IO TipoAlgebraico
pedirOperacion = do
    putStrLn "  Seleccione la operacion:"
    putStrLn "  1. Suma"
    putStrLn "  2. Resta"
    putStrLn "  3. Multiplicacion"
    putStrLn "  4. Division"
    opcion <- getLine
    case opcion of
        "1" -> return Suma
        "2" -> return Resta
        "3" -> return Multiplicacion
        "4" -> return Division
        _   -> do
            putStrLn "  Opcion invalida, ingrese 1, 2, 3 o 4"
            pedirOperacion

-- Menu principal recursivo
-- El usuario puede definir variables, evaluar expresiones o salir
menuPrincipal :: Env -> IO ()
menuPrincipal env = do
    putStrLn ""
    putStrLn "----- MENU PRINCIPAL -----"
    putStrLn $ "Variables definidas: " ++ show (map fst env)
    putStrLn "1. Definir variable"
    putStrLn "2. Construir y evaluar expresion"
    putStrLn "3. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            nuevoEnv <- definirVariable env
            menuPrincipal nuevoEnv
        "2" -> do
            expr <- construirExpr env
            putStrLn ""
            putStrLn $ "Expresion construida: "  ++ show expr
            putStrLn $ "Variables usadas: "      ++ show (recolectarVariables expr)
            putStrLn $ procesarResultado (evaluar env expr)
            menuPrincipal env
        "3" -> putStrLn "SALIENDO"
        _   -> do
            putStrLn "Opcion invalida "
            menuPrincipal env

-- Punto de entrada del programa
main :: IO ()
main = do
    putStrLn "---- Interprete de lenguaje funcional ----"
    putStrLn "---- (Evaluador de expresiones aritmeticas) ----"
    menuPrincipal []

-- Conclusion
-- En este proyecto número 2, se amplió el proyecto 1 sobre evaluacion de expresiones 
-- Se pudo implementar el manejo de errores utilizando Either, que permite indicar a que se debio el fallo
-- Ademas, se agrego el soporte de variables, haciendo uso de un entorno para evaluar recursivamente, la funcion es buscarVar
-- En ese mismo sentido, se aplico composicion funcional (con .) en la funcion procesarResultado, y fold de forma explicita en la funcion evaluarTodas
-- Por otro lado, se definieron instancias de Show para las funciones EvalError y Expr, separando el IO de la logica
-- El menu principal tambien mantiene separacion entre la logica y el IO, siendo recursivo

