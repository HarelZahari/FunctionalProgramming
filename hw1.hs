-- Harel Zahari
-- 305494452

--Ex1
shiftString :: String -> String
shiftString s = if isEmptyString s then s else (last s) : init s

isEmptyString :: String -> Bool
isEmptyString s = s== ""

--Ex2
isShifted :: String -> String -> Bool
isShifted s1 s2 = if length s1 == length s2 then checkIfIsVersionOfShiftedString s1 s2 (length s1) else False

checkIfIsVersionOfShiftedString :: String -> String -> Int -> Bool
checkIfIsVersionOfShiftedString s1 s2 0 = if s1 == "" && s2 == "" then True else False
checkIfIsVersionOfShiftedString s1 s2 n =  
    if s1 == shiftString s2 then True else checkIfIsVersionOfShiftedString (shiftString s1) s2 (n-1)

--Ex3
stupidListOp :: [Int] -> [Int]
stupidListOp [] = []
stupidListOp (x:xs) = (createSubStupidList x x) ++ (stupidListOp xs)

createSubStupidList :: Int -> Int -> [Int]
createSubStupidList n1 n2 = if n2 > 0 then n1 : createSubStupidList n1 (n2-1) else []

--Ex4
pascalLine :: Int -> [Int]
pascalLine lastLine = createPascalLine lastLine 0

createPascalLine :: Int -> Int -> [Int]
createPascalLine line index = if index <= line then (factorial line) `div` (factorial (line-index) * factorial index) : createPascalLine line (index + 1) else []

factorial :: Int -> Int
factorial n = if n > 0 then n * factorial (n-1) else 1

--Ex5

--Ex 5.1
toDigits :: Integer -> [Integer]
toDigits digits = if digits <= 0 then [] else paddingDigitsWithZeros(createDigitsList digits)

createDigitsList :: Integer -> [Integer]
createDigitsList digits = if digits <= 0 then [] else createDigitsList (div digits 10) ++ [mod digits 10]

paddingDigitsWithZeros :: [Integer] -> [Integer]
paddingDigitsWithZeros digits = if length digits == 9 then digits else paddingDigitsWithZeros (0:digits)  

--Ex 5.2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther listForDoubleEveryOther = createDoubleEveryOther 0 listForDoubleEveryOther

createDoubleEveryOther :: Int -> [Integer] -> [Integer]
createDoubleEveryOther index listForDoubleEveryOther = if index >= length listForDoubleEveryOther then [] else
     if mod index 2 == 1 then (listForDoubleEveryOther !! index * 2) : createDoubleEveryOther (index + 1) listForDoubleEveryOther
     else listForDoubleEveryOther !! index : createDoubleEveryOther (index + 1) listForDoubleEveryOther

--Ex 5.3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = getSumOfNumber x + sumDigits xs

getSumOfNumber :: Integer -> Integer
getSumOfNumber numForSum = if numForSum < 10 then numForSum else (numForSum `mod` 10) + (getSumOfNumber (numForSum `div` 10))

--Ex 5.4
validate :: Integer -> Bool
validate idForValidation = (sumDigits(doubleEveryOther(toDigits(idForValidation))) `mod` 10) == 0