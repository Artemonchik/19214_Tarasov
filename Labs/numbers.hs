import Data.Char
import Data.List
--Переводит строковое представление snumber из системы по основанию base в строковое представление числа в десятичной системе
getIntegerFromLetter :: Char -> Integer
getIntegerFromLetter char | (ord char) >= 97 && (ord char) <= 122 = toInteger $ ord char - 87
                      | ord char >= 48 && (ord char) <= 57 = toInteger $ ord char - 48
                      | (ord char) >= 65 && (ord char) <= 90 = toInteger $ ord char - 29
                      | otherwise = error "this is invalid char"


getLetterFromInteger :: Integer -> Char
getLetterFromInteger integer | integer >= 0 && integer <= 9 = chr (fromIntegral integer + 48)
                             | integer >= 10 && integer <= 35 =  chr ( fromIntegral integer + 87)
                             | integer >= 36  && integer <= 61 =  chr (fromIntegral integer + 29)
                             | otherwise = error "this integer can\'t be represented as char"

toDecimal :: Integer -> String -> String 
toDecimal base snumber | base >= 1 && base <= 61 = show (answer intArr)
                       | otherwise = error "base is invalid"
                          where intArr = map getIntegerFromLetter snumber 
                                answer [] = 0  
                                answer (x:intArr)  | x == 0 && base == 1 = error "char is not valid for this base" 
                                                   | x == 1 && base == 1 =  x * (base ^ (toInteger $ length intArr)) + answer intArr 
                                                   | x < base = x * (base ^ (toInteger $ length intArr)) + answer intArr 
                                                   | otherwise =  error "char is not valid for this base"

fromDecimal::Integer -> String -> String 
fromDecimal toBase snumber | any (\char -> ord char < 48 || ord char > 57) snumber = error "incorrect number"
                           | toBase <= 61 && toBase > 1 = reverse $ map getLetterFromInteger (answer number)
                           | toBase == 1 = reverse $ map getLetterFromInteger (answerForBase1 number)
                           | otherwise = error "invalid base"
                    where 
                        number = read snumber
                        answerForBase1 0 = []
                        answerForBase1 n = 1:(answerForBase1 (n - 1))
                        answer 0 = []
                        answer number = number `mod` toBase:(answer $ number `div` toBase) 

convertFromTo :: Integer -> Integer -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase $ toDecimal fromBase snumber 