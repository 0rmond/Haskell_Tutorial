{-
Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.

You may assume that each input would have exactly one solution, and you may not use the same element twice.

You can return the answer in any order.
-}


checkSumToTarget :: Int -> Int -> Int -> Bool
checkSumToTarget x y target =
    (x + y) == target

compareHeadToTail :: Int -> [Int] -> Int -> Int -> Int
compareHeadToTail h t target count
    | t == [] = -1
    | checkSumToTarget h (head t) target = count
    | otherwise = (compareHeadToTail h (tail t) target (count+1))

reduceUntilTargetSumFound :: [Int] -> Int -> Int -> [Int]
reduceUntilTargetSumFound xs target count
    | xs == [] = []
    | iterIndex > -1 = [count, count+iterIndex+1]
    | otherwise = reduceUntilTargetSumFound (tail xs) target (count+1)
    where iterIndex = compareHeadToTail (head xs) (tail xs) target 0

-- TESTS --
testTwoSum :: [Int] -> Int -> [Int] -> Bool
testTwoSum testCase target expected = (expected == reduceUntilTargetSumFound testCase target 0)

runTests :: [[Int]] -> [Int] -> [[Int]] -> [Bool]-> [Bool]
runTests (c:cs) (t:ts) (e:es) rs
    | cs == [] && ts == [] && es == [] = ((testTwoSum c t e):rs)
    | otherwise = runTests cs ts es (((testTwoSum c t e)):rs)

cases :: [[Int]]
cases = [ [2,7,11,15], [3,2,4], [3,3] ]
targets :: [Int]
targets = [ 9, 6, 6 ]
expectedResults :: [[Int]]
expectedResults = [ [0,1], [1,2], [0,1] ]

main = do
    print (runTests cases targets expectedResults [])
