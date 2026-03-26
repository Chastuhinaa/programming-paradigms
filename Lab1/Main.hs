import System.IO


countItem :: Int -> [Int] -> Int
countItem item list = length [x | x <- list, x == item]


removeTriples :: [Int] -> [Int] -> [Int]
removeTriples list1 list2 = [x | x <- list1, countItem x list2 /= 3]

isValidNumber :: String -> Bool
isValidNumber "" = False
isValidNumber "-" = False
isValidNumber ('-':xs) = length [c | c <- xs, c `elem` "0123456789"] == length xs
isValidNumber xs       = length [c | c <- xs, c `elem` "0123456789"] == length xs

allValid :: [String] -> Bool
allValid ws = length [w | w <- ws, isValidNumber w] == length ws

processSecondList :: Bool -> [Int] -> [String] -> IO ()
processSecondList False _ _ = putStrLn "\nПомилка: Ми не працюємо з такими типами даних! Вводьте лише числа."
processSecondList True list1 words2 = do
    let list2 = map read words2 :: [Int]
    putStrLn "\nВаш перший список:"
    print list1
    putStrLn "Ваш другий список:"
    print list2
    putStrLn "\nРезультат :"
    print (removeTriples list1 list2)

processFirstList :: Bool -> [String] -> IO ()
processFirstList False _ = putStrLn "\nПомилка: Ми не працюємо з такими типами даних! Вводьте лише числа."
processFirstList True words1 = do
    let list1 = map read words1 :: [Int]
    putStrLn "Введіть другий список чисел через пробіл :"
    input2 <- getLine
    let words2 = words input2
    
    processSecondList (allValid words2) list1 words2

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8

    
    putStrLn "Введіть перший список чисел через пробіл :"         
    input1 <- getLine
    let words1 = words input1
    
    processFirstList (allValid words1) words1