import System.IO

-- Convierte números a palabras
numberToWords :: Int -> String
numberToWords n
  | n >= 0 && n <= 19 = basicNumbers n -- Maneja números del 0 al 19 directamente
  | n >= 20 && n < 100 = tens (n `div` 10) ++ if n `mod` 10 /= 0 then "-" ++ basicNumbers (n `mod` 10) else "" -- Combina decenas y unidades para números entre 20 y 99
  | n == 100 = "One Hundred" -- Caso especial para 100
  | otherwise = "Número fuera de rango" -- Maneja números fuera del rango esperado
  where
    basicNumbers num = case num of -- Define la representación en palabras de números básicos
      0 -> "Zero"
      1 -> "One"
      2 -> "Two"
      3 -> "Three"
      4 -> "Four"
      5 -> "Five"
      6 -> "Six"
      7 -> "Seven"
      8 -> "Eight"
      9 -> "Nine"
      10 -> "Ten"
      11 -> "Eleven"
      12 -> "Twelve"
      13 -> "Thirteen"
      14 -> "Fourteen"
      15 -> "Fifteen"
      16 -> "Sixteen"
      17 -> "Seventeen"
      18 -> "Eighteen"
      19 -> "Nineteen"
    tens t = case t of -- Define la representación en palabras de las decenas
      2 -> "Twenty"
      3 -> "Thirty"
      4 -> "Forty"
      5 -> "Fifty"
      6 -> "Sixty"
      7 -> "Seventy"
      8 -> "Eighty"
      9 -> "Ninety"

-- Aplica las reglas del juego FizzBuzz
fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz!" -- Múltiplo de 3 y 5
  | n `mod` 5 == 0 = "Fizz!" -- Múltiplo de 5
  | n `mod` 3 == 0 = "Buzz!" -- Múltiplo de 3
  | otherwise = numberToWords n -- Cualquier otro número se convierte a palabras

-- Función principal que interactúa con el usuario
main :: IO ()
main = do
  putStrLn "Ingrese un número entre 0 y 100:" -- Solicita al usuario ingresar un número
  input <- getLine -- Lee la entrada del usuario
  let number = read input :: Int -- Convierte la entrada a un número entero
  if number >= 0 && number <= 100 -- Verifica si el número está en el rango válido
    then putStrLn $ fizzBuzz number -- Muestra el resultado de aplicar FizzBuzz o convertir a palabras
    else putStrLn "El número está fuera de rango." -- Indica si el número está fuera del rango permitido
