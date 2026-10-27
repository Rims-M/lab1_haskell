import Test.HUnit
--6.uzdevums

transposeMatrix :: [[Int]] -> [[Int]]
transposeMatrix [] = []
transposeMatrix rows =
    case splitRows rows of
        Nothing -> []
        Just (firsts, rests) ->
            firsts : transposeMatrix rests

splitRows :: [[Int]] -> Maybe ([Int], [[Int]])
splitRows [] = Just ([], [])
splitRows ([]:_) = Nothing
splitRows ((x:xs):rows) =
    case splitRows rows of
        Nothing -> Nothing
        Just (firsts, rests) ->
            Just (x : firsts, xs : rests)

tests :: Test
tests = 
    TestList
    [ TestLabel "parasta_matrica"
        (TestCase (assertEqual "rezultats" [[1, 4], [2, 5], [3, 6]]
            (transposeMatrix [[1, 2, 3],[4, 5, 6]]))), 

    TestLabel "1_rindas_matrica"
        (TestCase (assertEqual "rezultats" [[1], [2], [3]]
            (transposeMatrix [[1, 2, 3]]))),

    TestLabel "1_kolonnas_matrica"
        (TestCase (assertEqual "rezultats" [[1, 2, 3]]
            (transposeMatrix [[1], [2], [3]]))),

    TestLabel "tuksa_matrica"
        (TestCase (assertEqual "rezultats" []
            (transposeMatrix []))),

    TestLabel "kvadrat_matrica"
        (TestCase (assertEqual "rezultats" [[1, 3], [2, 4]]
            (transposeMatrix [[1, 2], [3, 4]])))


    ]



main :: IO ()
main = do
    let matrix = [[1, 2, 3], [4, 5, 6]]
    putStrLn ("sakotneja matrica" ++ show matrix)
    putStrLn ("transponeta matrica" ++ show (transposeMatrix matrix))

    counts <- runTestTT tests

    if errors counts + failures counts == 0 
        then return ()
        else error "HUnit testi neizdevas"