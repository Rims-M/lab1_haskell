import Data.Char (toUpper)
import Test.HUnit
-- 3. uzdevums
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeSentence :: String -> String
capitalizeSentence sentence = 
    unwords(foldr (\word acc -> capitalizeWord word : acc) [] (words sentence))

tests :: Test
tests = 
    TestList
        [ TestLabel "parasts_teikums"
            (TestCase (assertEqual "rezultats" "Hello World"
                (capitalizeSentence "hello world"))),

        TestLabel "Haskell Ir Labs"
            (TestCase (assertEqual "rezultats" "Haskell Ir Labs"
                (capitalizeSentence "haskell ir labs"))),

        TestLabel "viens vards"
            (TestCase (assertEqual "rezultats" "Latvija"
                (capitalizeSentence "latvija"))),

        TestLabel "tuksa_virkne"
            (TestCase (assertEqual "rezultats" ""
                (capitalizeSentence ""))),

        TestLabel "ir_kads_lielais_burts"
            (TestCase (assertEqual "rezultats" "Chau Bambino Cepolino"
                (capitalizeSentence "chau Bambino Cepolino")))

        ]



main ::IO ()
main = do
    putStrLn "ievadi teikumu:"
    sentence <- getLine
    putStrLn("rezultats: " ++ capitalizeSentence sentence)

    counts <- runTestTT tests

    if errors counts + failures counts == 0
        then return ()
        else error "HUnit testi neizdevas"