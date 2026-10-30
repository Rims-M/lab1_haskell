import Test.HUnit
{-# ANN module "HLint: ignore Use ++" #-}
{-# ANN module "HLint: ignore Use concatMap" #-}
--default (Int)

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

double :: Int -> Int
double x = x * 2

addTen :: Int -> Int
addTen x = x + 10


tests :: Test
tests =
    TestList
    [TestLabel "parasts_saraksts"
        (TestCase (assertEqual "rezultats" 
        (map double (concat [[1, 2], [3, 4]]))
            (concat (map (map double) [[1, 2], [3, 4]])))),

    TestLabel "tukss_saraksts"
        (TestCase (assertEqual "rezultats"
            (map double (concat ([] :: [[Int]])))
                (concat (map (map double) ([] :: [[Int]]))))), 

    TestLabel "viens_saraksts_tukss"
        (TestCase (assertEqual "rezultats"
        (map double (concat [[], [3, 4]]))
        (concat (map (map double) [[], [3, 4]])))),

    TestLabel "negativs_saraksts"
        (TestCase (assertEqual "rezultats" 
        (map double (concat [[-1, -2], [-3, -4]]))
            (concat (map (map double) [[-1, -2], [-3, -4]])))),

    TestLabel "jauns_saraksts"
        (TestCase (assertEqual "rezultats" 
        (map addTen (concat [[1, 2], [3, 4]]))
            (concat (map (map addTen) [[1, 2], [3, 4]]))))

    ]

main :: IO ()
main = do
    --let lists = [[1, 2], [3, 4]]
    --let f x = x * 2

    let lists :: [[Int]]
        lists = [[1, 2], [3, 4]]

    let f :: Int -> Int
        f x = x * 2

    let leftSide = map f (concat lists)
    let rightSide = concat (map (map f) lists)

    putStrLn ("kreisa puse: " ++ show leftSide)
    putStrLn ("laba puse: " ++ show rightSide)
    putStrLn ("vai abas puses vienadas: " ++ show (leftSide == rightSide))

    counts <- runTestTT tests

    if errors counts + failures counts == 0
        then return ()
        else error "HUnit testi neizdevas"