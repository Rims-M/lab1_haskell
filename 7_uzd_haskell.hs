import Test.HUnit
--7.uzdevums

data Tree a = Branch a (Tree a) (Tree a) | Leaf
isSearchTree :: Ord a => Tree a -> Bool
isSearchTree tree = checkTree Nothing Nothing tree

checkTree :: Ord a => Maybe a -> Maybe a -> Tree a -> Bool
checkTree _ _ Leaf = True

checkTree minVal maxVal (Branch x left right) = 
    validMin &&
    validMax &&
    checkTree minVal (Just x) left &&
    checkTree (Just x) maxVal right
    where

        validMin = case minVal of
            Nothing -> True 
            Just minV -> x > minV

        validMax = case maxVal of
            Nothing -> True
            Just maxV -> x <= maxV

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree e Leaf = Branch e Leaf Leaf
insertTree e (Branch x left right)
    | e <= x = Branch x (insertTree e left) right
    | e > x  = Branch x left (insertTree e right)

insertTreeWrong :: Ord a => a -> Tree a -> Tree a
insertTreeWrong e Leaf = Branch e Leaf Leaf
insertTreeWrong e (Branch x left right)
    | e <= x = Branch x (insertTreeWrong e left) right
    | e > x  = Branch x (insertTreeWrong e left) right   


goodTree :: Tree Int
goodTree = 
    Branch 8
        (Branch 4 Leaf Leaf)
        (Branch 12 Leaf Leaf)

badTree :: Tree Int
badTree = 
    Branch 8
        (Branch 10 Leaf Leaf)
        (Branch 12 Leaf Leaf)

tests :: Test
tests = 
    TestList
    [TestLabel "labs_koks"
        (TestCase (assertEqual "rezultats" True
            (isSearchTree goodTree))), 

    TestLabel "slikts_koks"
        (TestCase (assertEqual "rezultats" False
            (isSearchTree badTree))),

    TestLabel "pareizs_koks"
        (TestCase (assertEqual "rezultats" True
            (isSearchTree (insertTree 6 goodTree)))),

    TestLabel "ievietot_nepareiza_koka"
        (TestCase (assertEqual "rezultats" False
            (isSearchTree (insertTreeWrong 10 goodTree)))),

    TestLabel "tukss_koks"
        (TestCase (assertEqual "rezultats" True
            (isSearchTree (Leaf :: Tree Int))))

    ]

main :: IO ()
main = do
    putStrLn ("pareizs koks:" ++ show (isSearchTree goodTree))
    putStrLn ("nepareizs koks: " ++ show (isSearchTree badTree))

    let newTree = insertTree 6 goodTree
    putStrLn ("koks pec 6 ievietosanas: " ++ show (isSearchTree newTree))

    let wrongTree = insertTreeWrong 10 goodTree
    putStrLn ("koks ar nepareizu insertTree: " ++ show (isSearchTree wrongTree))

    counts <- runTestTT tests

    if errors counts + failures counts == 0
        then return ()
        else error "HUnit testi nestrada"