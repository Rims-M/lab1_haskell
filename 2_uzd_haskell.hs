import Test.HUnit

--2.uzdevums

reverseWord :: String -> String
reverseWord [] = []
reverseWord (x:xs) = reverseWord xs ++ [x]

tests :: Test
tests =
    TestList
    [ TestLabel "parasts_vards"
        (TestCase (assertEqual "rezultas" "olleh"
            (reverseWord "hello"))),

      TestLabel "viens_simbols"
        (TestCase (assertEqual "rezultats" "a"
            (reverseWord "a"))),   
    
      TestLabel "tuksa_virkne"
            (TestCase (assertEqual "rezultats" ""
                (reverseWord ""))),
    
      TestLabel "palindroms"
            (TestCase (assertEqual "rezultats" "anna"
                (reverseWord "anna"))),
        
      TestLabel "burti-un_cipari"
            (TestCase (assertEqual "rezultats" "321cba"
                (reverseWord "abc123")))


    ]

main :: IO ()
main = do
    putStrLn "ievadi vardu:"
    word <- getLine
    putStrLn("apgriezts vards ir " ++ reverseWord word)

    counts <- runTestTT tests

    if errors counts + failures counts == 0
        then return ()
        else error "HUnit testi neizdevas"