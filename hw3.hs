-- Harel Zahari
-- 305494452

-- Naor Zaharia
-- 312423841

module HW3 where
 
--Ex1
data NZDigit = One | Two | Three deriving Show
data Digit = Zero | NZ NZDigit deriving Show
data Base4Num = Single Digit | Multiple NZDigit [Digit] deriving Show

toBase4 :: Base4Num -> String
toBase4 (Single singleDigit) = [getDigitString singleDigit]
toBase4 (Multiple singleNZDigit digitsList) = (getNZDigitString singleNZDigit) : (map getDigitString digitsList)

getDigitString :: Digit -> Char
getDigitString (Zero) = '0'
getDigitString (NZ nzDigit) = getNZDigitString nzDigit

getNZDigitString :: NZDigit -> Char
getNZDigitString (One) = '1'
getNZDigitString (Two) = '2'
getNZDigitString (Three) = '3'

--Ex2
toNum :: Base4Num -> Int
toNum (Single singleDigit) = getBase4DigitToInt singleDigit
toNum (Multiple singleNZDigit digitsList) = ((getNZDigitToInt singleNZDigit) * (4 ^ (length digitsList))) + (createBase4NumToInt ((length digitsList)-1) digitsList)

createBase4NumToInt :: Int -> [Digit] -> Int
createBase4NumToInt _ [] = 0
createBase4NumToInt currentIndex (x:xs) = ((getBase4DigitToInt x) * (4 ^ currentIndex)) + createBase4NumToInt (currentIndex - 1) xs

getBase4DigitToInt :: Digit -> Int
getBase4DigitToInt (Zero) = 0
getBase4DigitToInt (NZ nzDigit) = getNZDigitToInt nzDigit

getNZDigitToInt :: NZDigit -> Int
getNZDigitToInt (One) = 1
getNZDigitToInt (Two) = 2
getNZDigitToInt (Three) = 3

--Ex3
fromBase4 :: String -> Base4Num
fromBase4 base4NumString = if ((length base4NumString)==1)
                                then Single (createBase4Digit (head base4NumString))
                           else
                                Multiple (createBase4NZDigit (head base4NumString)) (map createBase4Digit (tail base4NumString))

createBase4Digit :: Char -> Digit
createBase4Digit stringDigit = if stringDigit=='0'
                                    then Zero
                               else NZ (createBase4NZDigit stringDigit)

createBase4NZDigit :: Char -> NZDigit
createBase4NZDigit stringDigit = if stringDigit=='1'
                                    then One
                                 else if stringDigit=='2'
                                          then Two
                                      else Three

--Ex4
fromNum :: Int -> Base4Num
fromNum 0 = fromBase4 "0"
fromNum number = (fromBase4 . createIntToBase4String) number

createIntToBase4String :: Int -> String
createIntToBase4String 0 = []
createIntToBase4String currentNumber = createIntToBase4String (currentNumber `div` 4) ++ [(getBase4NumberChar (currentNumber `mod` 4))]

getBase4NumberChar :: Int -> Char
getBase4NumberChar 0 = '0'
getBase4NumberChar 1 = '1'
getBase4NumberChar 2 = '2'
getBase4NumberChar 3 = '3'

--Ex5
type Count = Int
data CNode t = Item Count t deriving Show
data CBinTree t = NULL | Node (CBinTree t) (CNode t) (CBinTree t) deriving Show

addItem :: (Ord t) => (CBinTree t) -> t -> (CBinTree t)
addItem NULL input = Node NULL (Item 1 input) NULL
addItem (Node left (Item counter value) right) input = if(value == input)
                                        then Node left (Item (counter+1) value) right
                                        else if(value > input)
                                            then Node (addItem left input) (Item counter value) right
                                            else Node left (Item counter value) (addItem right input)

--Ex6
maxCount :: (Ord t) => (CBinTree t) -> (Maybe t ,Int)
maxCount NULL = (Nothing, 0)
maxCount (Node left (Item counter value) right) = findMaxCountTuple [maxCount left, maxCount right, (Just value, counter)] (Nothing,0)

findMaxCountTuple :: [(Maybe t ,Int)] -> (Maybe t, Int) -> (Maybe t ,Int)
findMaxCountTuple [] maxTuple = maxTuple
findMaxCountTuple (x:xs) maxTuple = if(snd maxTuple < snd x)
                                    then findMaxCountTuple xs x
                                    else findMaxCountTuple xs maxTuple