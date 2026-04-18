--17.Виявити, чи існує таке натуральне число k, що заданий скінчений автомат не допускає жодного слова довжини k. При ствердній відповіді вивести значення відповідного k.
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List       (intercalate)
import Data.Char       (isSpace)
import qualified Data.Set as Set
import System.IO       (hSetEncoding, stdout, stdin, utf8)

data FA = FA
    { faStates      :: [String]
    , faAlphabet    :: String
    , faTransitions :: [(String, Char, String)]
    , faInitial     :: String
    , faAccepting   :: [String]
    }

type StateSet = Set.Set String

stepFA :: FA -> StateSet -> StateSet
stepFA fa current =
    Set.fromList
        [ to
        | (from, _a, to) <- faTransitions fa
        , Set.member from current
        ]

findGap :: FA -> Maybe Int
findGap fa = go 1 s1 [s0]
  where
    acceptSet = Set.fromList (faAccepting fa)
    s0 = Set.singleton (faInitial fa)
    s1 = stepFA fa s0
    go k cur seen
        | Set.null (cur `Set.intersection` acceptSet) = Just k
        | cur `elem` seen                             = Nothing
        | otherwise = go (k+1) (stepFA fa cur) (cur : seen)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

prompt :: String -> IO String
prompt msg = putStr msg >> fmap trim getLine

hline :: IO ()
hline = putStrLn (replicate 60 '-')

showTransitions :: [(String, Char, String)] -> IO ()
showTransitions [] = putStrLn "    (немає переходiв)"
showTransitions ts = mapM_ (\(q,a,q') ->
    putStrLn $ "    d(" ++ q ++ ", " ++ [a] ++ ") = " ++ q') ts

showFA :: FA -> IO ()
showFA fa = do
    putStrLn $ "  Стани Q    = { " ++ intercalate ", " (faStates fa) ++ " }"
    putStrLn $ "  Алфавiт S  = { " ++ intercalate ", " (map (:[]) (faAlphabet fa)) ++ " }"
    putStrLn $ "  Поч. стан  = " ++ faInitial fa
    putStrLn $ "  Фiн. стани = { " ++ intercalate ", " (faAccepting fa) ++ " }"
    putStrLn   "  Переходи:"
    showTransitions (faTransitions fa)

showMaybe :: Maybe Int -> String
showMaybe Nothing  = "k не iснує"
showMaybe (Just k) = "k = " ++ show k

printResult :: FA -> IO ()
printResult fa = do
    putStrLn ">>> Введений автомат:"
    showFA fa
    putStrLn ""
    putStrLn ">>> РЕЗУЛЬТАТ:"
    showResult (findGap fa)

showResult :: Maybe Int -> IO ()
showResult Nothing = do
    putStrLn "    Такого натурального числа k НЕ iснує."
    putStrLn "    Для кожного k >= 1 автомат допускає"
    putStrLn "    хоча б одне слово довжини k."
showResult (Just k) = do
    putStrLn $ "    Знайдено k = " ++ show k
    putStrLn $ "    Автомат не допускає жодного слова довжини " ++ show k ++ "."

showStatus :: Bool -> String
showStatus True  = "  [OK]"
showStatus False = "  [FAIL] Тест НЕ пройдено!"

runTest :: Int -> String -> FA -> Maybe Int -> IO Bool
runTest num desc fa expected = do
    hline
    putStrLn $ "ТЕСТ " ++ show num ++ ": " ++ desc
    hline
    showFA fa
    putStrLn ""
    let actual = findGap fa
    putStrLn $ "  Очiкується : " ++ showMaybe expected
    putStrLn $ "  Отримано   : " ++ showMaybe actual
    putStrLn ""
    let status = actual == expected
    putStrLn (showStatus status)
    putStrLn ""
    return status

-- Тест 1: автомат приймає тiльки слово "ab"
-- S1={q1}, {q1}∩{q2}=∅  →  k=1
test1 :: (String, FA, Maybe Int)
test1 = ( "Автомат приймає лише слово 'ab' (k=1)"
        , FA { faStates      = ["q0","q1","q2"]
             , faAlphabet    = "ab"
             , faTransitions = [("q0",'a',"q1"),("q1",'b',"q2")]
             , faInitial     = "q0"
             , faAccepting   = ["q2"]
             }
        , Just 1
        )

-- Тест 2: автомат приймає всi непорожнi слова над {a,b}
-- S1={q1}, цикл  →  Nothing
test2 :: (String, FA, Maybe Int)
test2 = ( "Всi непорожнi слова над {a,b} — k не iснує"
        , FA { faStates      = ["q0","q1"]
             , faAlphabet    = "ab"
             , faTransitions = [("q0",'a',"q1"),("q0",'b',"q1")
                               ,("q1",'a',"q1"),("q1",'b',"q1")]
             , faInitial     = "q0"
             , faAccepting   = ["q1"]
             }
        , Nothing
        )

-- Тест 3: слова парної довжини над {a}
-- S1={O}, {O}∩{E}=∅  →  k=1
test3 :: (String, FA, Maybe Int)
test3 = ( "Слова парної довжини над {a} (k=1)"
        , FA { faStates      = ["E","O"]
             , faAlphabet    = "a"
             , faTransitions = [("E",'a',"O"),("O",'a',"E")]
             , faInitial     = "E"
             , faAccepting   = ["E"]
             }
        , Just 1
        )

-- Тест 4: слова довжини кратної 3 над {a}
-- S1={B}, {B}∩{C}=∅  →  k=1
test4 :: (String, FA, Maybe Int)
test4 = ( "Слова довжини кратної 3 над {a} (k=1)"
        , FA { faStates      = ["A","B","C"]
             , faAlphabet    = "a"
             , faTransitions = [("A",'a',"B"),("B",'a',"C"),("C",'a',"A")]
             , faInitial     = "A"
             , faAccepting   = ["C"]
             }
        , Just 1
        )

-- Тест 5: мертвий автомат — немає переходiв
-- S1={} (порожня)  →  k=1
test5 :: (String, FA, Maybe Int)
test5 = ( "Мертвий автомат: 0 переходiв (k=1)"
        , FA { faStates      = ["q0"]
             , faAlphabet    = "a"
             , faTransitions = []
             , faInitial     = "q0"
             , faAccepting   = ["q0"]
             }
        , Just 1
        )

-- Тест 6: тотальний автомат — один стан, петля
-- S1={q0}, цикл  →  Nothing
test6 :: (String, FA, Maybe Int)
test6 = ( "Тотальний автомат над {a,b}: один стан — k не iснує"
        , FA { faStates      = ["q0"]
             , faAlphabet    = "ab"
             , faTransitions = [("q0",'a',"q0"),("q0",'b',"q0")]
             , faInitial     = "q0"
             , faAccepting   = ["q0"]
             }
        , Nothing
        )

-- Тест 7: немає фiнальних станiв
-- S1 ∩ {} = ∅  →  k=1
test7 :: (String, FA, Maybe Int)
test7 = ( "Немає фiнальних станiв (F=порожня) — k=1"
        , FA { faStates      = ["q0","q1"]
             , faAlphabet    = "ab"
             , faTransitions = [("q0",'a',"q1"),("q1",'b',"q0")]
             , faInitial     = "q0"
             , faAccepting   = []
             }
        , Just 1
        )

-- Тест 8: k=2
-- S1={q1}∩{q1}≠∅, S2={q2}∩{q1}=∅  →  k=2
test8 :: (String, FA, Maybe Int)
test8 = ( "k=2: S1 перетинається з F, але S2 — нi"
        , FA { faStates      = ["q0","q1","q2"]
             , faAlphabet    = "a"
             , faTransitions = [("q0",'a',"q1"),("q1",'a',"q2"),("q2",'a',"q2")]
             , faInitial     = "q0"
             , faAccepting   = ["q1"]
             }
        , Just 2
        )

-- Тест 9: автомат розпiзнає слова, що мiстять "aa"
-- S1={B,A}, {B,A}∩{C}=∅  →  k=1
test9 :: (String, FA, Maybe Int)
test9 = ( "Автомат розпiзнає слова, що мiстять 'aa' (k=1)"
        , FA { faStates      = ["A","B","C"]
             , faAlphabet    = "ab"
             , faTransitions = [("A",'a',"B"),("A",'b',"A")
                               ,("B",'a',"C"),("B",'b',"A")
                               ,("C",'a',"C"),("C",'b',"C")]
             , faInitial     = "A"
             , faAccepting   = ["C"]
             }
        , Just 1
        )

-- Тест 10: k=3
-- S1={q1}∩F≠∅, S2={q2}∩F≠∅, S3={q3}∩F=∅  →  k=3
test10 :: (String, FA, Maybe Int)
test10 = ( "k=3: S1,S2 перетинаються з F, але S3 — нi"
         , FA { faStates      = ["q0","q1","q2","q3"]
              , faAlphabet    = "a"
              , faTransitions = [("q0",'a',"q1"),("q1",'a',"q2")
                                ,("q2",'a',"q3"),("q3",'a',"q3")]
              , faInitial     = "q0"
              , faAccepting   = ["q1","q2"]
              }
         , Just 3
         )

allTests :: [(String, FA, Maybe Int)]
allTests = [test1, test2, test3, test4, test5
           ,test6, test7, test8, test9, test10]

runAllTests :: IO ()
runAllTests = do
    putStrLn (replicate 60 '=')
    putStrLn "  АВТОМАТИЧНI ТЕСТИ"
    putStrLn (replicate 60 '=')
    putStrLn ""
    results <- mapM (\(i,(desc,fa,exp)) -> runTest i desc fa exp)
                    (zip [1..] allTests)
    putStrLn (replicate 60 '=')
    let passed = length (filter id results)
        total  = length results
    putStrLn $ "  ПIДСУМОК: " ++ show passed ++ " / " ++ show total ++ " тестiв пройдено"
    putStrLn (replicate 60 '=')
    putStrLn ""

readFA :: IO FA
readFA = do
    hline
    putStrLn "  Введення скiнченного автомата"
    hline
    putStrLn ""
    putStrLn "Крок 1. Введiть усi стани автомата через пробiл."
    putStrLn "  Приклад:  q0 q1 q2"
    statesLine <- prompt "  Стани: "
    let sts = words statesLine
    putStrLn ""
    putStrLn "Крок 2. Введiть символи алфавiту без пробiлiв."
    putStrLn "  Приклад:  ab   або   01"
    alphaLine <- prompt "  Алфавiт: "
    let alpha = filter (not . isSpace) alphaLine
    putStrLn ""
    putStrLn "Крок 3. Введiть початковий стан."
    initSt <- prompt "  Початковий стан: "
    putStrLn ""
    putStrLn "Крок 4. Введiть фiнальнi стани через пробiл."
    putStrLn "  (якщо фiнальних станiв немає — просто натиснiть Enter)"
    accLine <- prompt "  Фiнальнi стани: "
    let acc = words accLine
    putStrLn ""
    putStrLn "Крок 5. Введiть кiлькiсть переходiв."
    nStr <- prompt "  Кiлькiсть переходiв: "
    let n = read nStr :: Int
    putStrLn ""
    putStrLn "Крок 6. Введiть переходи."
    putStrLn "  Формат:  стан_звiдки  символ  стан_куди"
    putStrLn "  Приклад: q0 a q1"
    trs <- readTransitions n 1
    return FA { faStates = sts, faAlphabet = alpha
              , faTransitions = trs, faInitial = initSt
              , faAccepting = acc }

readTransitions :: Int -> Int -> IO [(String, Char, String)]
readTransitions 0 _ = return []
readTransitions remaining idx = do
    line <- prompt ("  Перехiд " ++ show idx ++ ": ")
    case words line of
        [from, [sym], to] -> do
            rest <- readTransitions (remaining - 1) (idx + 1)
            return ((from, sym, to) : rest)
        [from, sym, to] | length sym == 1 -> do
            rest <- readTransitions (remaining - 1) (idx + 1)
            return ((from, head sym, to) : rest)
        _ -> do
            putStrLn "  ! Невiрний формат. Потрiбно: стан символ стан"
            readTransitions remaining idx

continueLoop :: String -> IO () -> IO ()
continueLoop answer action
    | answer `elem` ["т","Т","y","Y","так","yes"] = action
    | otherwise = return ()

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin  utf8
    putStrLn (replicate 60 '=')
    putStrLn "  Лабораторна робота 3. Роздiл 3. Варiант 17."
    putStrLn "  Задача: знайти натуральне k таке, що скiнченний"
    putStrLn "  автомат не допускає жодного слова довжини k."
    putStrLn (replicate 60 '=')
    putStrLn ""
    runAllTests
    again <- prompt "Ввести власний автомат? (т/н): "
    continueLoop again interactiveLoop

interactiveLoop :: IO ()
interactiveLoop = do
    putStrLn ""
    fa <- readFA
    putStrLn ""
    printResult fa
    putStrLn ""
    again <- prompt "Ввести ще один автомат? (т/н): "
    continueLoop again interactiveLoop
