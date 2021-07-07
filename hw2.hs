-- Harel Zahari
-- 305494452

-- Naor Zaharia
-- 312423841

module HW2 where
       
--Ex1
makeDistinctInt :: [Int] -> [Int]
makeDistinctInt [] = []
makeDistinctInt [x] = [x]
makeDistinctInt intList = makeIntDistinctAccumulator intList []

makeIntDistinctAccumulator :: [Int] -> [Int] -> [Int]
makeIntDistinctAccumulator [] accumulator = accumulator
makeIntDistinctAccumulator (x:xs) accumulator = if((isIntListContain x accumulator) == False)
                                                then makeIntDistinctAccumulator xs (accumulator++[x])
                                                else makeIntDistinctAccumulator xs accumulator

isIntListContain :: Int -> [Int] -> Bool
isIntListContain _ [] = False
isIntListContain a (x:xs) = if (a==x)
                            then True
                            else isIntListContain a xs

--Ex2
makeDistinctString :: String -> [Char]
makeDistinctString s = makeStringDistinctAccumulator s []

makeStringDistinctAccumulator :: [Char] -> [Char] -> [Char]
makeStringDistinctAccumulator [] accumulator = accumulator
makeStringDistinctAccumulator (x:xs) accumulator = if((isStringListContain x accumulator) == False)
                                                then makeStringDistinctAccumulator xs (accumulator++[x])
                                                else makeStringDistinctAccumulator xs accumulator

isStringListContain :: Char -> [Char] -> Bool
isStringListContain _ [] = False
isStringListContain a (x:xs) = if (a==x)
                            then True
                            else isStringListContain a xs

--Ex3
multiplyLists :: [Int] -> [Int] -> [(Int,Int)]
multiplyLists s1 s2 = createCrossProductList s1 s2

createCrossProductList :: [a] -> [b] -> [(a,b)]
createCrossProductList _ [] = []
createCrossProductList [] _ = []
createCrossProductList (x:xs) s2 = (zip (replicate (length s2) x) s2) ++ createCrossProductList xs s2

--Ex4
listSquares :: Int -> [Int]
listSquares n = createListOfAllSquaresLessthanInputNumber n 1 []

createListOfAllSquaresLessthanInputNumber :: Int -> Int -> [Int] -> [Int]
createListOfAllSquaresLessthanInputNumber n currentIndex accumulator = if (currentIndex*currentIndex <= n)
                                                                          then createListOfAllSquaresLessthanInputNumber n (currentIndex + 1) (accumulator++[(currentIndex*currentIndex)])
                                                                            else accumulator

--Ex5
roundSquare :: Int -> Int
roundSquare n = 
                let squareListN = listSquares n
                    squareListNPlusOne = listSquares ((length(squareListN) + 1)*(length(squareListN) + 1))
                in 
                    if((last squareListNPlusOne)-n < n-(last squareListN))
                        then length squareListNPlusOne
                        else length squareListN

--Ex6
twoSqaures :: Int -> (Int, Int)
twoSqaures n = findWhichTupleIsTheSumSquaresOfInput n (multiplyLists (listSquares n) (listSquares n))

findWhichTupleIsTheSumSquaresOfInput :: Int -> [(Int,Int)] -> (Int,Int)
findWhichTupleIsTheSumSquaresOfInput n (x:xs) =
                                                 if((fst x)+(snd x) == n)
                                                    then (roundSquare (fst x), roundSquare (snd x))
                                                    else findWhichTupleIsTheSumSquaresOfInput n xs

--Ex7
slowSort :: [Int] -> [Int]
slowSort listForSort = slowSortAccumulator listForSort []

slowSortAccumulator :: [Int] -> [Int] -> [Int]
slowSortAccumulator [] accumulator = accumulator
slowSortAccumulator listForSort accumulator = slowSortAccumulator (removeNumberFromList listForSort (minimum listForSort))  (accumulator++[minimum listForSort])

removeNumberFromList :: [Int] -> Int -> [Int]
removeNumberFromList (x:xs) elementForRemove = if(x==elementForRemove)
                                                then xs
                                                else removeNumberFromList (xs++[x]) elementForRemove