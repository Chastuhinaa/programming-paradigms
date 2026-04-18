--7. Розбити заданий список на N підсписків, записуючи в ці підсписки елементи по черзі.
{- HLINT ignore "Use if" -}
module Main where
import System.IO

myDrop :: Int -> [Int] -> [Int]
myDrop _ [] = []
myDrop n (x:xs)
    | n <= 0    = x:xs
    | otherwise = myDrop (n - 1) xs

takeEvery :: Int -> [Int] -> [Int]
takeEvery _ [] = []
takeEvery n (x:xs) = x : takeEvery n (myDrop (n - 1) xs)

makeSublists :: Int -> Int -> [Int] -> [[Int]]
makeSublists total current xs
    | current == total = []  
    | otherwise        = takeEvery total (myDrop current xs) : makeSublists total (current + 1) xs


splitList :: Int -> [Int] -> [[Int]]
splitList n xs
    | n <= 0    = [] 
    | otherwise = makeSublists n 0 xs


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

    
    putStrLn "Введіть кількість підсписків (N):"
    inputN <- getLine
    let wordsN = words inputN
    
    
    case length wordsN == 1 && isValidNumber (head wordsN) of
        False -> putStrLn "\n Помилка: N має бути одним цілим числом!"
        True  -> do
            let n = read (head wordsN) :: Int
            
            
            case n > 0 of
                False -> putStrLn "\n Помилка: Кількість підсписків має бути більшою за 0!"
                True  -> do
                   
                    putStrLn "Введіть список чисел через пробіл (наприклад: 1 2 3 4 5 6 7):"
                    inputList <- getLine
                    let stringList = words inputList
                    
                    
                    case allValid stringList of
                        False -> putStrLn "\n Помилка: Ми не працюємо з такими типами даних! Вводьте лише цілі числа."
                        True  -> do
                            let intList = map read stringList :: [Int]
                            
                            putStrLn "\nВаш початковий список:"
                            print intList
                            
                            putStrLn ("\nРезультат (розбито на " ++ show n ++ " підсписків):")
                            print (splitList n intList)