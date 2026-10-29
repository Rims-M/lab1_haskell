import Test.HUnit
--8.uzdevums

--map f . concat = concat . map (map f)

--Bazes gadijums: xss = []
--
--Kreisa puse:
--map f (concat [])
-- = map f []
-- = []
--
--Laba puse:
--  concat (map (map f) [])
-- = concat []
-- = []
--
--Abas puses ir  vienadas

--Indukcijas pienemums:
--map f (concat xss) = concat (map (map f) xss)
--
--Indukcijas solis:
--
--map f (concat (xs : xss))
-- = map f (xs ++ concat xss)
-- = map f xs ++ map f (concat xss)
-- = map f xs ++ concat (map (map f) xss)
-- = concat (map f xs : map (map f) xss)
-- = concat (map (map f) (xs : xss))
--
--Tatad:
-- map f . concat = concat . map (map f)

tests :: Test
tests =
    TestList
    [TestLabel "parasts_saraksts"
        (TestCase (assertEqual "rezultats" 
        (map (*2) (concat [[1, 2], [3, 4]]))
            (concat (map (map (*2)) [[1, 2], [3, 4]])))),

    TestLabel "tukss_saraksts"
        (TestCase (assertEqual "rezultats"
            (map (*2) (concat ([] :: [[Int]])))
                (concat (map (map (*2)) ([] :: [[Int]]))))), 

    TestLabel "viens_saraksts_tukss"
        (TestCase (assertEqual "rezultats"
        (map (*2) (concat [[], [3, 4]]))
        (concat (map (map (*2)) [[], [3, 4]])))),

    TestLabel "negativs_saraksts"
        (TestCase (assertEqual "rezultats" 
        (map (*2) (concat [[-1, -2], [-3, -4]]))
            (concat (map (map (*2)) [[-1, -2], [-3, -4]])))),

    TestLabel "jauns_saraksts"
        (TestCase (assertEqual "rezultats" 
        (map (+10) (concat [[1, 2], [3, 4]]))
            (concat (map (map (+10)) [[1, 2], [3, 4]]))))

    ]

main :: IO ()
main = do
    let lists = [[1, 2], [3, 4]]
    let f x = x * 2

    let leftSide = map f (concat lists)
    let rightSide = concat (map (map f) lists)

    putStrLn ("kreisa puse: " ++ show leftSide)
    putStrLn ("laba puse: " ++ show rightSide)
    putStrLn ("vai abas puses vienadas: " ++ show (leftSide == rightSide))

    counts <- runTestTT tests

    if errors counts + failures counts == 0
        then return ()
        else error "HUnit testi neizdevas"