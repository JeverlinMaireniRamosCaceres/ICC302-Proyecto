
-- Proyecto 2
-- Emil Paulino, ID: 10154329
-- Jeverlin Ramos, ID: 10154300


-- INTERPRETE DE LENGUAJE FUNCIONAL (EVALUADOR DE EXPRESIONES ARITMETICAS)
-- El evaluador de expresiones aritmeticas calcula el resultado de la expresion construida usando tipos algebraicos
-- utilizando recursion, manejo de errores con Either, y separacion de logica e IO


-- Evaluacion de errores
-- Tipo definido para poder representar los errores posibles durante la evaluacion de una expresion
-- Se utiliza Either para representar los exitos (con Right) y los fallos (con Left)
data EvalError = DivisionPorCero | VariableNoDefinida String
instance Show EvalError where
    show DivisionPorCero = "Error. Division por cero"
    show (VariableNoDefinida var) = "Error. La variable " ++ var ++ " no esta definida"


-- Tipos algebraicos
-- Definicion del tipo algebraico propio, solo es posible suma, resta, multiplicacion y division

-- Se define este tipo algebraico con las operaciones basicas aritmeticas de suma, resta, multiplicacion y division.
-- Esto con el proposito de realizar las operaciones y poder utilizar pattern matching

data Operacion = Suma | Resta | Multiplicacion | Division 

-- La funcion aplicarTipo recibe un operador (que es Operacion), y dos numeros a, y devuelve otro numero si todo es ejecutado correctamente
-- a: es un tipo de dato general, que representa que puede ser cualquier tipo de numero
-- Se utiliza Fractional a, Eq a para permitir operar con distintos tipos de numeros y que la division y la comparacion con cero sea correcta

aplicarTipo :: (Fractional a, Eq a) => Operacion -> a -> a -> Either EvalError a

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
-- lo que permite construir expresiones simples o anidadas
data Expr a = Valor a | Var String |Aplicar Operacion (Expr a) (Expr a)

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

-- Convierte una operacion a su simbolo matematico correspondiente
-- Funcion auxiliar pura que muestra el simbolo del operador usando instance Show Expr
showOp :: Operacion -> String
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

-- Suma todos los resultados validos de una lista de Either EvalError Double
-- Ignora los errores (Left) y va acumulando solo los valores exitosos (Right)
-- Usa foldr para recorrer la lista declarativamente
sumaResultadosValidos :: [Either EvalError Double] -> Double
sumaResultadosValidos = foldr sumar 0
  where
    sumar (Right x) acc = x + acc
    sumar (Left _)  acc = acc

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

-- Para garantizar la unicidad de las variables
-- Inserta una variable en el entorno garantizando que no haya duplicados
-- Si la variable ya existe, la reemplaza con el nuevo valor
insertarVariable :: String -> Double -> Env -> Env
insertarVariable nombre valor env =
    (nombre, valor) : filter (\(n, _) -> n /= nombre) env

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
            return (insertarVariable nombre val env)
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

-- Construye la expresion aritmetica de forma interactiva y recursiva
-- El usuario escoge primero el operando izquierdo, luego la operacion, luego el operando derecho
-- Si el usuario elige anidar, se llama a si misma de forma recursiva para construir la subexpresion
-- No evalua, unicamente construye el arbol de tipo Expr
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
pedirOperacion :: IO Operacion
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

-- Muestra el historial de expresiones evaluadas con el entorno actual
-- Usa evaluarTodas para evaluar nuevamente todas las expresiones con foldr
-- Muestra cada resultado usando procesarResultado y la suma de resultados validos
mostrarHistorial :: Env -> [Expr Double] -> IO ()
mostrarHistorial env historial = do
    putStrLn ""
    putStrLn "----- HISTORIAL -----"

    let resultados = evaluarTodas env historial
    let suma = sumaResultadosValidos resultados

    mapM_ (putStrLn . procesarResultado) resultados
    putStrLn $ "Suma de resultados validos: " ++ show suma

-- Menu principal recursivo
-- El usuario puede definir variables, evaluar expresiones o salir
menuPrincipal :: Env -> [Expr Double] -> IO ()
menuPrincipal env historial = do
    putStrLn ""
    putStrLn "----- MENU PRINCIPAL -----"
    putStrLn $ "Variables definidas: " ++ show (map fst env)
    putStrLn "1. Definir variable"
    putStrLn "2. Construir y evaluar expresion"
    putStrLn "3. Ver historial"
    putStrLn "4. Salir"
    opcion <- getLine

    case opcion of
        "1" -> do
            nuevoEnv <- definirVariable env
            menuPrincipal nuevoEnv historial

        "2" -> do
            expr <- construirExpr env
            putStrLn ""
            putStrLn $ "Expresion construida: " ++ show expr
            putStrLn $ "Variables usadas: " ++ show (recolectarVariables expr)
            putStrLn $ "Profundidad del arbol: " ++ show (profundidad expr)
            putStrLn $ procesarResultado (evaluar env expr)
            menuPrincipal env (historial ++ [expr])

        "3" -> do
            mostrarHistorial env historial
            menuPrincipal env historial

        "4" -> putStrLn "SALIENDO"

        _ -> do
            putStrLn "Opcion invalida"
            menuPrincipal env historial

-- MAIN, interaccion con el usuario
main :: IO ()
main = do
    putStrLn "---- Interprete de lenguaje funcional ----"
    putStrLn "---- (Evaluador de expresiones aritmeticas) ----"
    menuPrincipal [] []

-- Conclusion
-- En este proyecto se extendio el evaluador de expresiones del proyecto anterior.
-- Se incorporo manejo de errores mediante Either, lo que permite representar fallos
-- explicitamente y de forma segura. Ademas, se agrego soporte para variables por medio
-- de un entorno de evaluacion y una busqueda recursiva con buscarVar.
-- Tambien se utilizaron otras herramientas propias del paradigma funcional, como recursion
-- estructural sobre Expr, composicion de funciones en procesarResultado y foldr en
-- evaluarTodas y sumaResultadosValidos. Del mismo modo, se mantuvo una separacion clara
-- entre la logica pura y la interaccion con el usuario mediante IO.

{- 

EJEMPLOS DE COMO NAVEGAR EL MENU

===== EJEMPLO 1 =====

----- MENU PRINCIPAL -----
Variables definidas: []
1. Definir variable
2. Construir y evaluar expresion
3. Ver historial
4. Salir
1
Nombre de la variable:
x
Valor de la variable:
2
Variable x = 2.0 definida

----- MENU PRINCIPAL -----
Variables definidas: ["x"]
1. Definir variable
2. Construir y evaluar expresion
3. Ver historial
4. Salir
2

Primer valor:
  1. Numero
  2. Variable (disponibles: ["x"])
1
  Ingrese el numero:
3
Operacion:
  Seleccione la operacion:
  1. Suma
  2. Resta
  3. Multiplicacion
  4. Division
1
Segundo valor:
  1. Numero
  2. Variable (disponibles: ["x"])
  3. Otra operacion (anidar)
2
  Ingrese el nombre de la variable:
x

Expresion construida: (3.0 + x)
Variables usadas: ["x"]
Profundidad del arbol: 1
[Evaluacion] Resultado: 5.0

===== EJEMPLO 2 =====
---- Interprete de lenguaje funcional ----
---- (Evaluador de expresiones aritmeticas) ----

----- MENU PRINCIPAL -----
Variables definidas: []
1. Definir variable
2. Construir y evaluar expresion
3. Ver historial
4. Salir
1
Nombre de la variable:
x
Valor de la variable:
3
Variable x = 3.0 definida

----- MENU PRINCIPAL -----
Variables definidas: ["x"]
1. Definir variable
2. Construir y evaluar expresion
3. Ver historial
4. Salir
1
Nombre de la variable:
y
Valor de la variable:
2
Variable y = 2.0 definida

----- MENU PRINCIPAL -----
Variables definidas: ["y","x"]
1. Definir variable
2. Construir y evaluar expresion
3. Ver historial
4. Salir
2

Primer valor:
  1. Numero
  2. Variable (disponibles: ["y","x"])
1
  Ingrese el numero:
5
Operacion:
  Seleccione la operacion:
  1. Suma
  2. Resta
  3. Multiplicacion
  4. Division
1
Segundo valor:
  1. Numero
  2. Variable (disponibles: ["y","x"])
  3. Otra operacion (anidar)
3

Primer valor:
  1. Numero
  2. Variable (disponibles: ["y","x"])
2
  Ingrese el nombre de la variable:
x
Operacion:
  Seleccione la operacion:
  1. Suma
  2. Resta
  3. Multiplicacion
  4. Division
3
Segundo valor:
  1. Numero
  2. Variable (disponibles: ["y","x"])
  3. Otra operacion (anidar)
2
  Ingrese el nombre de la variable:
y

Expresion construida: (5.0 + (x * y))
Variables usadas: ["x","y"]
Profundidad del arbol: 2
[Evaluacion] Resultado: 11.0

-}