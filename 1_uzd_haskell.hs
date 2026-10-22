{-|
Funkcijas merkis
atrast lielako skaitli no saraksta

ievade
[Int] - skaitlu saraksts, kura tiek meklets lielakais skaitlis

izvade
Int - lielakais skaitlis no saraksta

Funkcija darbojas izmantojot rekursiju. Ja saraksta ir viens skaitlis,
tas tiek atgriezts ka rezultats. Ja skaitlu ir vairak,
pirmais skaitlis tiek salidzinats ar atlikusa saraksta
lielako skaitli, izmantojot funkciju 'max'.

Piemers:findMaxNumber [5, 10, 11, 15, 3, 8] == 15

-}

--import Data.Char (toUpper)
import           Test.HUnit
--1.uzdevums

findMaxNumber :: [Int] -> Int
findMaxNumber []     = error "Saraksts ir tukss"
findMaxNumber [x]    = x
findMaxNumber (x:xs) = max x (findMaxNumber xs)

--testNormalList :: Test
--testNormalList =
    --TestCase (assertEqual "Parasts saraksts" 15 (findMaxNumber [5, 10, 11, 15, 3, 8]))

tests :: Test
tests =
    TestList
        [ TestLabel "Parasts saraksts"
            (TestCase (assertEqual "Rezultats" 15
                (findMaxNumber [5, 10, 11, 15, 3, 8]))),

          TestLabel "Viens elements"
            (TestCase (assertEqual "Rezultats" 7
                (findMaxNumber [7]))),

          TestLabel "Negativi skaitli"
            (TestCase (assertEqual "Rezultats" (-2)
                (findMaxNumber [-5, -2, -10]))),

          TestLabel "Vienadi skaitli"
            (TestCase (assertEqual "Rezultats" 5
                (findMaxNumber [5, 5, 5]))),

          TestLabel "Maksimums sakuma"
            (TestCase (assertEqual "Rezultats" 20
                (findMaxNumber [20, 5, 10]))),

          TestLabel "Maksimums beigas"
            (TestCase (assertEqual "Rezultats" 20
                (findMaxNumber [5, 10, 20])))
        ]

main :: IO ()
main = do
    let numbers = [5, 10, 11, 15, 3, 8]
    putStrLn ("maksimalais skaitlis ir " ++ show  (findMaxNumber numbers))

    counts <- runTestTT tests

    if errors counts + failures counts == 0
        then return ()
        else error "HUnit testi neizdevas"
