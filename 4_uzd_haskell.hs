import Test.HUnit
-- 4.uzdevums
lastElement :: [Int] -> Int
lastElement [] = error "saraksts ir tukss"
lastElement [x] = x
lastElement (_:xs) = lastElement xs


tests :: Test
tests =
    TestList
    [ TestLabel "parasts_saraksts"
        (TestCase (assertEqual "rezultats" 20
            (lastElement [5, 10, 15, 20]))),

    TestLabel "viens_skaitlis_saraksta"
        (TestCase (assertEqual "rezultats" 7
            (lastElement [7]))), 

    TestLabel "negativi_skaitli"
        (TestCase (assertEqual "rezultats" (-15)
            (lastElement [-5, -10, -15]))), 

    TestLabel "vienadi skaitli"
        (TestCase (assertEqual "rezultats" 5
            (lastElement [5, 5, 5]))),

    TestLabel "pedejais elements"
        (TestCase (assertEqual "rezultats" 1
            (lastElement [10, 299, 1])))


    ]

main :: IO ()
main = do
    let numbers = [5, 10, 15, 20]
    putStrLn ("pedejais elements ir : " ++ show (lastElement numbers))

    counts <- runTestTT tests

    if errors counts + failures counts == 0 
        then return ()
        else error "HUnit testi neizdevas"