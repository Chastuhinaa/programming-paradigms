--2. Знайти максимальний елемент та переставити всі його примірники в початок списку.
{- HLINT ignore "Use if" -}
module Main where
import System.IO


findMax :: [Int] -> Int
findMax [x] = x
findMax (x:xs)
    | x > maxTail = x
    | otherwise   = maxTail
  where
    maxTail = findMax xs


moveMaxToFront :: [Int] -> [Int]
moveMaxToFront [] = []
moveMaxToFront list = maxElements ++ otherElements
  where
    m = findMax list
    maxElements = [x | x <- list, x == m]
    otherElements = [x | x <- list, x /= m]


isValidNumber :: String -> Bool
isValidNumber "" = False
isValidNumber "-" = False
isValidNumber ('-':xs) = length [c | c <- xs, c `elem` "0123456789"] == length xs
isValidNumber xs       = length [c | c <- xs, c `elem` "0123456789"] == length xs

allValid :: [String] -> Bool
allValid ws = length [w | w <- ws, isValidNumber w] == length ws


main :: IO ()
main = do
    
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8

    putStrLn "Введіть список чисел через пробіл (наприклад: 3 8 1 8 4 2):"         
    input <- getLine
    let stringList = words input
    
    
    case allValid stringList of
        False -> putStrLn "\n Помилка: Ми не працюємо з такими типами даних! Вводьте лише цілі числа."
        True  -> do
            
            case stringList of
                [] -> putStrLn "\n Помилка: Ви нічого не ввели!"
                _  -> do
                 
                    let intList = map read stringList :: [Int]
                    putStrLn "\nВаш початковий список:"
                    print intList
                    
                    putStrLn "\nРезультат (максимуми на початку):"
                    print (moveMaxToFront intList)