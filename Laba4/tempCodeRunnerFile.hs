module Main where


import Data.Map.Strict (Map)
import Data.Set        (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Data.List       (nub, intercalate, isPrefixOf)
import System.IO       (hSetEncoding, stdout, stdin, hFlush)
import GHC.IO.Encoding (utf8)

data Symbol = Term String | NonT String | Eps
  deriving (Eq, Ord)

instance Show Symbol where
  show (Term a) = a
  show (NonT a) = a
  show Eps      = "eps"

type Rule      = (String, [Symbol])
type Grammar   = [Rule]
type First1Map = Map String (Set Symbol)

data GrammarError
  = EmptyGrammar
  | EmptyNonterminal
  | EmptyRhs      String
  | DuplicateRule Rule
  | EmptyTerminal String
  deriving (Eq)

instance Show GrammarError where
  show EmptyGrammar =
    "ПОМИЛКА: граматика не містить жодного правила."
  show EmptyNonterminal =
    "ПОМИЛКА: є правило з порожнiм iменем нетермiнала."
  show (EmptyRhs nt) =
    "ПОМИЛКА: права частина правила [" ++ nt ++
    "] порожня (використайте eps для epsilon-правила)."
  show (DuplicateRule (n, r)) =
    "УВАГА: дублiкат правила [" ++ n ++ " -> " ++
    unwords (map show r) ++ "]."
  show (EmptyTerminal nt) =
    "ПОМИЛКА: правило [" ++ nt ++
    "] мiстить термiнал з порожнiм рядком."

validateGrammar :: Grammar -> [GrammarError]
validateGrammar [] = [EmptyGrammar]
validateGrammar g  = concat
  [ [EmptyNonterminal  | any (null . fst) g]
  , [EmptyRhs nt       | (nt, rhs) <- g, null rhs]
  , [EmptyTerminal nt  | (nt, rhs) <- g, Term "" `elem` rhs]
  , [DuplicateRule r   | r <- nub (duplicates g)]
  ]
  where
    duplicates xs = [x | x <- xs, length (filter (== x) xs) > 1]



allNonTerminals :: Grammar -> [String]
allNonTerminals g = nub (map fst g)

initFirst1 :: Grammar -> First1Map
initFirst1 g = Map.fromList [(nt, Set.empty) | nt <- allNonTerminals g]

firstOfRhs :: First1Map -> [Symbol] -> Set Symbol
firstOfRhs _    []              = Set.singleton Eps
firstOfRhs curr (Eps    : rest) = firstOfRhs curr rest
firstOfRhs _    (Term a : _)    = Set.singleton (Term a)
firstOfRhs curr (NonT b : rest) =
  let fb         = Map.findWithDefault Set.empty b curr
      withoutEps = Set.delete Eps fb
  in case Set.member Eps fb of
       True  -> Set.union withoutEps (firstOfRhs curr rest)
       False -> withoutEps

iterStep :: Grammar -> First1Map -> First1Map
iterStep g curr = foldr applyRule curr g
  where
    applyRule (nt, rhs) acc =
      let new = firstOfRhs acc rhs
          old = Map.findWithDefault Set.empty nt acc
      in Map.insert nt (Set.union old new) acc

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x =
  let x' = f x
  in case x' == x of
       True  -> x
       False -> fixpoint f x'

computeFirst1 :: Grammar -> First1Map
computeFirst1 g = fixpoint (iterStep g) (initFirst1 g)


parseSymbol :: String -> Symbol
parseSymbol "eps"          = Eps
parseSymbol s@(c:_)
  | c `elem` ['A'..'Z']   = NonT s
  | otherwise              = Term s
parseSymbol []             = Term ""


parseRuleSimple :: String -> Either String Rule
parseRuleSimple line =
  let ws = words line
  in case ws of
       (nt : "->" : rhsParts) ->
         let syms = map parseSymbol rhsParts
         in case null nt of
              True  -> Left "Порожнє iм'я нетермiнала."
              False -> Right (nt, syms)
       _ -> Left ("Неправильний формат рядка: \"" ++ line ++
                  "\"\nОчiкується: A -> символ1 символ2 ...")

showSet :: Set Symbol -> String
showSet s
  | Set.null s = "{}"
  | otherwise  = "{ " ++ intercalate ", " (map show (Set.toList s)) ++ " }"

printFirst1 :: First1Map -> IO ()
printFirst1 m = mapM_ go (Map.toAscList m)
  where
    go (nt, s) = putStrLn ("  first1(" ++ nt ++ ") = " ++ showSet s)

printGrammar :: Grammar -> IO ()
printGrammar g = mapM_ go g
  where
    go (nt, rhs) =
      putStrLn ("  " ++ nt ++ " -> " ++ unwords (map show rhs))

separator :: IO ()
separator = putStrLn (replicate 54 '-')


runBuiltinTest :: String -> Grammar -> IO ()
runBuiltinTest name g = do
  separator
  putStrLn ("Тест: " ++ name)
  putStrLn "Граматика:"
  printGrammar g
  let errs = validateGrammar g
  case errs of
    [] -> do
      putStrLn "Результат first1:"
      printFirst1 (computeFirst1 g)
    _  -> do
      putStrLn "Знайденi проблеми:"
      mapM_ (\e -> putStrLn ("  [!] " ++ show e)) errs

grammar1 :: Grammar
grammar1 =
  [ ("S", [Term "a", NonT "B"])
  , ("B", [Term "b"])
  , ("B", [Eps]) ]

grammar2 :: Grammar
grammar2 =
  [ ("S", [NonT "A", NonT "B"])
  , ("A", [Term "a"])
  , ("A", [Eps])
  , ("B", [Term "b"])
  , ("B", [Term "c"]) ]

grammar3 :: Grammar
grammar3 =
  [ ("S", [NonT "A", NonT "B"])
  , ("A", [Eps])
  , ("B", [Eps]) ]

grammar4 :: Grammar
grammar4 =
  [ ("S", [NonT "S", Term "a"])
  , ("S", [Term "b"]) ]

grammar5 :: Grammar
grammar5 =
  [ ("A", [NonT "B"])
  , ("B", [NonT "C"])
  , ("C", [Term "d"])
  , ("C", [Eps]) ]

grammar6 :: Grammar
grammar6 =
  [ ("E",  [NonT "T",  NonT "E'"])
  , ("E'", [Term "+",  NonT "T", NonT "E'"])
  , ("E'", [Eps])
  , ("T",  [NonT "F",  NonT "T'"])
  , ("T'", [Term "*",  NonT "F", NonT "T'"])
  , ("T'", [Eps])
  , ("F",  [Term "(",  NonT "E", Term ")"])
  , ("F",  [Term "id"]) ]

grammar7 :: Grammar
grammar7 =
  [ ("A", [NonT "B", Term "a"])
  , ("A", [Term "c"])
  , ("B", [NonT "A", Term "b"])
  , ("B", [Term "d"])
  , ("B", [Eps]) ]

grammarBad1 :: Grammar
grammarBad1 = [("S", [Term "a"]), ("B", [])]

grammarBad2 :: Grammar
grammarBad2 = [("S",[Term "a"]),("S",[Term "b"]),("S",[Term "a"])]

grammarBad3 :: Grammar
grammarBad3 = []

grammarBad4 :: Grammar
grammarBad4 = [("S", [Term ""])]

runAllBuiltin :: IO ()
runAllBuiltin = do
  putStrLn "\n=== Вбудованi тести ==="
  runBuiltinTest "S->a B, B->b|eps"               grammar1
  runBuiltinTest "S->A B, A->a|eps, B->b|c"       grammar2
  runBuiltinTest "S->A B, A->eps, B->eps"          grammar3
  runBuiltinTest "Лiворекурсивна: S->S a|b"       grammar4
  runBuiltinTest "Ланцюг: A->B->C->d|eps"          grammar5
  runBuiltinTest "Арифметичнi вирази (E,T,F)"     grammar6
  runBuiltinTest "Взаємна рекурсiя A<->B"         grammar7
  runBuiltinTest "ПОМИЛКА: порожня права частина"  grammarBad1
  runBuiltinTest "УВАГА: дублiкат правила"         grammarBad2
  runBuiltinTest "ПОМИЛКА: порожня граматика"      grammarBad3
  runBuiltinTest "ПОМИЛКА: термiнал \"\""          grammarBad4
  separator

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

interactiveMode :: IO ()
interactiveMode = do
  putStrLn ""
  putStrLn "=== Введення власної граматики ==="
  putStrLn "Формат кожного правила:  A -> символ1 символ2 ..."
  putStrLn "  - Нетермiнал: починається з великої лiтери (S, A, E, E')"
  putStrLn "  - Термiнал:   починається з малої або є спецсимволом (+, *, id)"
  putStrLn "  - Epsilon:    слово  eps"
  putStrLn "Введiть правила по одному. Порожнiй рядок = кiнець введення."
  putStrLn ""
  rules <- collectRules []
  case rules of
    [] -> putStrLn "[!] Не введено жодного правила."
    _  -> do
      putStrLn "\nВведена граматика:"
      printGrammar rules
      let errs = validateGrammar rules
      case errs of
        [] -> do
          putStrLn "\nРезультат first1:"
          printFirst1 (computeFirst1 rules)
        _  -> do
          putStrLn "\nЗнайденi проблеми:"
          mapM_ (\e -> putStrLn ("  [!] " ++ show e)) errs

collectRules :: Grammar -> IO Grammar
collectRules acc = do
  line <- prompt (show (length acc + 1) ++ "> ")
  case line of
    "" -> return (reverse acc)
    _  ->
      case parseRuleSimple line of
        Left  err  -> do
          putStrLn ("  [!] " ++ err)
          collectRules acc
        Right rule -> do
          putStrLn ("  OK: " ++ fst rule ++ " -> " ++
                    unwords (map show (snd rule)))
          collectRules (rule : acc)


mainMenu :: IO ()
mainMenu = do
  putStrLn ""
  
  putStrLn "  1 - Запустити всi вбудованi тести"
  putStrLn "  2 - Ввести власну граматику"
  putStrLn "  0 - Вийти"
  putStrLn "------------------------------------------"
  choice <- prompt "Ваш вибiр: "
  case choice of
    "1" -> do
      runAllBuiltin
      mainMenu
    "2" -> do
      interactiveMode
      again <- prompt "\nЗнову ввести граматику? (т/н): "
      case again of
        "т" -> do interactiveMode; mainMenu
        "y" -> do interactiveMode; mainMenu
        _   -> mainMenu
    "0" -> putStrLn "До побачення!"
    _   -> do
      putStrLn "[!] Невiдома команда. Введiть 1, 2 або 0."
      mainMenu

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin  utf8
  mainMenu