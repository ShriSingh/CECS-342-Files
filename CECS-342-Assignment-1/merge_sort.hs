merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
    where
        (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs

main :: IO ()
main = do
        let arr = [5, 2, 9, 3, 8, 7]
        if null arr
          then putStrLn "List is Empty"
          else do
            putStrLn "Test Case #1 list: "
            print arr
            putStrLn "Sorted list using quicksort: "
            print (mergeSort arr)
        
        let arr2 = [6, 3, 2, 5, 8, 1]
        if null arr2
          then putStrLn "List is Empty"
          else do
            putStrLn "\nTest Case #2 list: "
            print arr2
            putStrLn "Sorted list using quicksort: "
            print (mergeSort arr2)
    
        let arr3 = [33, 83, 91, 71, 35, 3, 16, 57, 31]
        if null arr3
          then putStrLn "List is Empty"
          else do
            putStrLn "\nTest Case #3 list: "
            print arr3
            putStrLn "Sorted list using quicksort: "
            print (mergeSort arr3)
    
        let arr4 = []
        if null arr4
          then putStrLn "\nList is Empty"
          else do
            putStrLn "\nTest Case #4 list: "
            putStrLn arr4
            putStrLn "Sorted list using quicksort: "
            putStrLn (mergeSort arr4)