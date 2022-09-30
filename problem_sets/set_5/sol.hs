{-# OPTIONS_GHC -Wall #-}
import CodeWorld

--1.a
binaryToDecimal :: [Int] -> Int
binaryToDecimal bits = helper (reverse bits) 0 0
  where
    helper [] result power = result
    helper (x : xs) result power = helper xs (result + (x * (2 ^ power))) (power + 1)


--1.b
leadingZeros :: [Int] -> Int
leadingZeros [] = 0
leadingZeros (0 : xs) = 1 + leadingZeros xs
leadingZeros (1 : xs) = 0

countZeros :: [Int] -> Int
countZeros bits = helper bits - leadingZeros bits
  where
    helper [] = 0
    helper (0 : xs) = 1 + helper xs
    helper (1 : xs) = helper xs 


--1.c
removeLeadingZeros :: [Int] -> [Int]
removeLeadingZeros bits = helper bits (leadingZeros bits)
  where
    helper bits 0 = bits
    helper (_ : xs) n = helper xs (n - 1)

encodeWithLengths :: [Int] -> [Int]
encodeWithLengths bits = helper (removeLeadingZeros bits) 0 []
  where
    helper (x : []) currentChunk result = (result ++ [currentChunk + 1])
    helper (x : y : xs) currentChunk result
      | x == y = helper (y : xs) (currentChunk + 1) result
      | otherwise = helper (y : xs) 0 (result ++ [currentChunk + 1])


--1.d
binaryOdd :: [Int] -> Bool
binaryOdd bits = helper bits
  where
    helper (x : [])
      | x == 1 = True
      | x == 0 = False   
    helper (x : xs) = helper xs

--1.e
trailingZeros :: [Int] -> Int
trailingZeros bits = leadingZeros (reverse bits)

removeTrailingZeros :: [Int] -> [Int]
removeTrailingZeros bits = helper (reverse bits) (trailingZeros bits)
  where
    helper bits 0 = (reverse bits)
    helper (_ : xs) n = helper xs (n - 1)

decrement :: [Int] -> [Int]
decrement (x : []) = [0]
decrement bits = removeLeadingZeros (helper (trailingZeros bits) (reverse (removeTrailingZeros bits)))
  where
    helper trailingZ (x : xs) = addOnesHelper (0 : xs) trailingZ
      where 
        addOnesHelper bits 0 = reverse bits
        addOnesHelper bits num = addOnesHelper (1 : bits) (num - 1)


--1.f
type BoolInt = (Bool, Int)

propagate :: (Bool, [Int]) -> [BoolInt]
propagate (val, bits) = helper val bits []
  where
    helper val [] result = result
    helper val (x : xs) result = helper val xs (result ++ [(val, x)])



--2.a
alternatingSum :: [Int] -> Int
alternatingSum bits = helper bits 0 0
  where
    helper [] flag mySum = mySum
    helper (x : xs) 0 mySum = helper xs 1 (mySum + x)
    helper (x : xs) flag mySum = helper xs 0 (mySum - x)

--2.b
-- alternatingSum [1,2,3,4,5]
-- = helper bits 0 0
--    where bits = [1,2,3,4,5]
-- = helper [1,2,3,4,5] 0 0
-- = helper xs 1 (mySum + x)
--    where xs = [2, 3, 4, 5], mySum = 0, x = 1
-- = helper [2, 3, 4, 5] 1 (0 + 1)
-- = helper [2, 3, 4, 5] 1 1
-- = helper xs 0 (mySum - x)
--    where xs = [3, 4, 5], mySum = 1, x = 2
-- = helper [3, 4, 5] 0 (1 - 2)
-- = helper [3, 4, 5] 0 -1
-- = helper xs 1 (mySum + x)
--    where xs = [4, 5], mySum = -1, x = 3
-- = helper [4, 5] 1 (-1 + 3)
-- = helper [4, 5] 1 2
-- = helper xs 0 (mySum - x)
--    where xs = [5], mySum = 2, x = 4
-- = helper [5] 0 (2 - 4)
-- = helper [5] 0 -2
-- = helper xs 1 (mySum + x)
--    where xs = [], mySum = -2, x = 5
-- = helper [] 1 (-2 + 5)
-- = helper [] 1 3
-- = mySum
--   where mySum = 3
-- = 3


--3
data Degrees = Degrees Double

data Radians = Radians Double

toDegrees :: Radians -> Degrees
toDegrees (Radians radians) =  Degrees ((radians * 180) / pi)

fromDegrees :: Degrees -> Radians
fromDegrees (Degrees degrees) =  Radians ((degrees * pi) / 180)

main :: IO ()
main = print (alternatingSum [1,2,3,4,5])