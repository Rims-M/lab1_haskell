import Test.HUnit
--5.uzdevums

decode :: [(Int, Int)] -> [Int]
decode [] = []
decode ((count, value):xs) = replicate count value ++ decode xs

tests :: Test
tests =
    TestList
    [ TestLabel "parasts_saraksts"
        (TestCase (assertEqual "rezultats" [5, 5, 5]
            (decode [(3, 5)]))),

    TestLabel "kodeti_pari"
        (TestCase (assertEqual "rezultats" [3, 3, 5, 5 ]
            (decode [(2, 3), (2, 5)]))),

    TestLabel "viens_paris"
        (TestCase (assertEqual "rezultats" [4, 8, 10]
            (decode [(1, 4), (1, 8), (1, 10)]))),

    TestLabel "tukss_saraksts"
        (TestCase (assertEqual "rezultats" []
            (decode []))),

    TestLabel "negativi_skaitli"
        (TestCase (assertEqual "rezultats" [-5, -5, -10]
            (decode [(2, -5), (1, -10)])))


    ]


main :: IO ()
main = do
    let encoded = [(3,5), (2,8), (1,10)]
    putStrLn ("atjaunotais saraksts ir: " ++ show (decode encoded))

    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then return ()
        else error "HUnit testi neizdevas"