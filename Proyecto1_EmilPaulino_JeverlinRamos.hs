
-- Proyecto 2
-- Emil Paulino, ID: 10154329
-- Jeverlin Ramos, ID: 10154300


-- EVALUADOR DE EXPRESIONES ARITMETICAS
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

-- Funcion que evalua de forma recursiva una expresion y maneja errores usando Maybe
evalua :: (Fractional a, Eq a) => Expr a -> Either EvalError a

-- Evalua de forma recursiva una expresion y retorna Either EvalError a
-- Si ambas subexpresiones evaluan correctamente (Right), se aplica el operador
-- Si alguna subexpresion falla, se propaga el error especifico con Left
-- El caso Var retorna OperacionInvalida porque evalua no tiene acceso al entorno

evalua (Valor n) = Right n
evalua (Var _) = Left OperacionInvalida
evalua (Aplicar op exp1 exp2) =
  combinar (evalua exp1) (evalua exp2)
  where
    combinar (Right valor1) (Right valor2) = aplicarTipo op valor1 valor2
    combinar (Left err) _ = Left err
    combinar _ (Left err) = Left err

-- Función que evalúa la operación aritmética según la opción del usuario
-- evaluarOperacion :: String -> Double -> Double -> Maybe Double

-- evaluarOperacion recibe tres parametros:
-- 1. Una cadena (String) que representa la operacion elegida por el usuario ("1" para suma, "2" para resta, etc.)
-- 2. Un numero de tipo Double, que sera el primer operando
-- 3. Un numero de tipo Double, que sera el segundo operando
-- La funcion devuelve un resultado de tipo Maybe Double:
-- Just resultado si la operacion se puede realizar correctamente
-- Nothing si hubo un error, por ejemplo division por cero o una opcion invalida

{-evaluarOperacion "1" x y = evalua (Aplicar Suma (Valor x) (Valor y))
evaluarOperacion "2" x y = evalua (Aplicar Resta (Valor x) (Valor y))
evaluarOperacion "3" x y = evalua (Aplicar Multiplicacion (Valor x) (Valor y))
evaluarOperacion "4" x y = evalua (Aplicar Division (Valor x) (Valor y))
evaluarOperacion _ _ _   = Nothing-}

-- Recolectar variables
-- Recorre el arbol de expresiones de forma recursiva y recolecta los nombres de todas las variables usadas
-- Es una funcion pura, no depende de IO ni del entorno, solo del arbol de expresiones
-- Caso base Valor: un numero no tiene variables, retorna lista vacia
-- Caso base Var: encontro una variable, retorna su nombre en una lista
-- Caso recursivo Aplicar: recorre ambos lados del arbol y une los resultados
recolectarVariables :: Expr a -> [String]
recolectarVariables (Valor _) = []
recolectarVariables (Var nombre) = [nombre]
recolectarVariables (Aplicar _ exp1 exp2) = recolectarVariables exp1 ++ recolectarVariables exp2


-- Interaccion con el usuario
-- Separa la logica y el IO. 
-- Se muestran las opciones del menu, se obtiene la opcion y se reciben los numeros ingresados por el usuario. 
-- Se hace uso de let para conventir los numeros (que son recibidos como tipo String) al tipo Double, con el fin de mantener la logica
-- separada de los efectos. 

{-main :: IO ()
main = do
    putStrLn "Seleccione una operación:"
    putStrLn "1. Suma"
    putStrLn "2. Resta"
    putStrLn "3. Multiplicación"
    putStrLn "4. División"

    operacion <- getLine
    putStrLn "Ingrese el primer número:"
    num1 <- getLine
    putStrLn "Ingrese el segundo número:"
    num2 <- getLine

    let x = read num1 :: Double
        y = read num2 :: Double
        resultado = evaluarOperacion operacion x y

    print resultado-}


-- Conclusion


