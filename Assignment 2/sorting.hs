{-
  Writing a sorting algorithm that can handle any data type 
  Then test the algorithm with test cases
    - sort numbers ascending by numerical value,
    - sort people alphabetically (lexicographically) by name, and to
    - sort people descending by age, where people of the same age should be sorted alphabetically (lexicographically).
-}

import Data.List (sortBy)

--Allows us to create a data type called Person with the parameters of name and age
data Person = Person { name :: String, age :: Int } deriving (Show, Eq, Ord)

numbers :: [Float]
numbers = [645.41, 37.59, 76.41, 5.31, -34.23, 1.11, 1.10, 23.46, 635.47, -876.32, 467.83, 62.25]

-- Assigns the variable people to the type Person
people :: [Person]
--Creates an array of people
people = [Person "Hal" 20, Person "Susann" 31, Person "Dwight" 19, Person "Kassandra" 21, Person "Lawrence" 25,
          Person "Cindy" 22, Person "Cory" 27, Person "Mac" 19, Person "Romana" 27, Person "Doretha" 32,
          Person "Danna" 20, Person "Zara" 23, Person "Rosalyn" 26, Person "Risa" 24, Person "Benny" 28, Person "Juan" 33,
          Person "Natalie" 25]

main :: IO ()
main = do
    -- Sorts by ascending order by comparing
    let sortedNumbers = sortBy (\a b -> compare a b) numbers 
    putStrLn "Sorted numbers (ascending):"
    print sortedNumbers
    putStrLn ""

    -- Sorts by comparing names
    let sortedPeopleByName = sortBy (\a b -> compare (name a) (name b)) people 
    putStrLn "Sorted people (alphabetically by name):"
    print sortedPeopleByName
    putStrLn ""

    -- Sorts by alphabetical order after checking if ages are the same
    let sortedPeopleByAgeDesc = sortBy (\a b -> if age a /= age b then compare (age b) (age a) else compare (name a) (name b)) people 
    putStrLn "Sorted people (descending by age, then alphabetically by name):"
    print sortedPeopleByAgeDesc