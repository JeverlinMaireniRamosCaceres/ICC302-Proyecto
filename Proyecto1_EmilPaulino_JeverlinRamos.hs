
-- Proyecto 1
-- Emil Paulino, ID: 10154329
-- Jeverlin Ramos, ID: 10154300

-- Evaluador de expresiones aritmeticas
-- El evaluador de expresiones aritmeticas calcula el resultado de la expresion construida usando tipos algebraicos
-- utilizando recursion, manejo de errores con Maybe, y separacion de logica e IO

-- Tipos algebraicos

-- Definicion del tipo algebraico propio, solo es posible suma, resta, multiplicacion y division

-- Se define este tipo algebraico con las operaciones basicas aritmeticas de suma, resta, multiplicacion y division. Esto con el proposito de realizar las operaciones
-- y poder utilizar pattern matching

data TipoAlgebraico = Suma | Resta | Multiplicacion | Division 

-- La funcion aplicarTipo recibe un operador (que es TipoAlgebraico), y dos numeros a, y devuelve otro numero si todo es ejecutado correctamente

-- a: es un tipo de dato general, que representa que puede ser cualquier tipo de numero

-- Se utiliza Fractional a, Eq a para permitir operar con distintos tipos de numeros y que la division y la comparacion con cero sea correcta

aplicarTipo :: (Fractional a, Eq a) => TipoAlgebraico -> a -> a -> Maybe a

-- Definicion de las funciones con pattern matching

-- Suma
aplicarTipo Suma x y = Just (x + y)

-- Resta 
aplicarTipo Resta x y = Just (x - y)

-- Multiplicacion
aplicarTipo Multiplicacion x y = Just (x * y)

-- Division por cero
aplicarTipo Division _ 0 = Nothing

-- Division normal
aplicarTipo Division x y = Just (x / y)

-- Definir expresion recursiva

-- Tipo de dato que representa una expresion aritmetica, un valor o una operacion aplicada a dos expresiones
-- La expresion aritmetica a evaluar puede ser un valor sencillo o una operacion que se aplique a dos expresiones,
-- lo que permite construir expresiones complejas a partir de expresiones pequeñas
data Expr a = Valor a | Aplicar TipoAlgebraico (Expr a) (Expr a)

-- Funcion que evalua de forma recursiva una expresion y maneja errores usando Maybe
evalua :: (Fractional a, Eq a) => Expr a -> Maybe a

-- Evalua de forma recursiva una expresion. Si las dos expresiones tienen valor, se aplica el operador, si alguna falla, retorna Nothing
-- La funcion --evalua-- evalua una expresion aritmetica recursivamente
-- Primero evalua las expresiones de la izquierda y de la derecha
-- Si ambas retornan un valor (Just), se aplica el operador (ya sea suma, resta, multiplicacion o division)
-- Si alguna de estas expresiones falla (Nothing), la evaluacion retorna Nothing

evalua (Valor n) = Just n
evalua (Aplicar op exp1 exp2) =
  combinar (evalua exp1) (evalua exp2)
  where
    combinar (Just valor1) (Just valor2) = aplicarTipo op valor1 valor2
    combinar _ _ = Nothing

-- Función que evalúa la operación aritmética según la opción del usuario
evaluarOperacion :: String -> Double -> Double -> Maybe Double

-- evaluarOperacion recibe tres parámetros:
-- 1. Una cadena (String) que representa la operación elegida por el usuario ("1" para suma, "2" para resta, etc.)
-- 2. Un número de tipo Double, que será el primer operando
-- 3. Un número de tipo Double, que será el segundo operando
-- La función devuelve un resultado de tipo Maybe Double:
-- Just resultado si la operación se puede realizar correctamente
-- Nothing si hubo un error, por ejemplo división por cero o una opción inválida

evaluarOperacion "1" x y = evalua (Aplicar Suma (Valor x) (Valor y))
evaluarOperacion "2" x y = evalua (Aplicar Resta (Valor x) (Valor y))
evaluarOperacion "3" x y = evalua (Aplicar Multiplicacion (Valor x) (Valor y))
evaluarOperacion "4" x y = evalua (Aplicar Division (Valor x) (Valor y))
evaluarOperacion _ _ _   = Nothing

-- Interaccion con el usuario
-- Separa la logica y el IO. 
-- Se muestran las opciones del menú, se obtiene la opción y se reciben los números ingresados por el usuario. 
-- Se hace uso de let para conventir los números (que son recibidos como tipo String) al tipo Double, con el fin de mantener la lógica
-- separada de los efectos. 

main :: IO ()
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

    print resultado

-- Conclusion

-- Al desarrollar este evaluador de expresiones aritméticas, pudimos aplicar el contenido visto en clase sobre la programación funcional.
-- Usando tipos algebraicos se modeló el tipo de dato que permite la estructura de las expresiones, permitiendo separar el modelo y la evaluación.
-- En ese mismo sentido, la función recursiva permitió recorrer y aplicar las operaciones aplicando los operadores según los escogiera el usuario, 
-- y los errores siendo manejados con Maybe. 
-- Además, se aplicaron los conceptos de pattern matching y la separación de la interacción con el usuario y la lógica pura.  

