import System.IO


countItem :: Int -> [Int] -> Int
countItem item list = length [x | x <- list, x == item]


removeTriples :: [Int] -> [Int] -> [Int]
removeTriples list1 list2 = [x | x <- list1, countItem x list2 /= 3]


main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8

    
    putStrLn "Введіть перший список чисел через пробіл :"         
    input1 <- getLine
    let list1 = map read (words input1) :: [Int]
    
    putStrLn "Введіть другий список чисел через пробіл :"
    input2 <- getLine
    let list2 = map read (words input2) :: [Int]

    
    putStrLn "\nВаш перший список:"
    print list1
    
    putStrLn "Ваш другий список:"
    print list2
    
    putStrLn "\nРезультат :"
    let result = removeTriples list1 list2
    print result