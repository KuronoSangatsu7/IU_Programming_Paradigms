-- Jaffar Totanji - SD-01 - j.totanji@innopolis.university

import CodeWorld

type Name = String
data Grade = A | B | C | D
data Student = Student Name Grade

data Result a
  = Success a
  | Failure String

-- 2

studentsWithA :: [Student] -> [Name]
studentsWithA students = helper students []
  where
    helper [] result = result
    helper (Student x A : xs) result = helper xs (result ++ [x])
    helper (_ : xs) result = helper xs result

-- 3.a
f n 
    | n > 100 = Failure "input is too large"
    | otherwise = Success (2 * n)
    
extractSuccessValue :: Result a -> a
extractSuccessValue (Success val) = val
 
extractFailureValue :: Result a -> String
extractFailureValue (Failure val) = val 

isSuccessful :: Result a -> Bool
isSuccessful (Success a) = True
isSuccessful _ = False 

whileSuccess :: (a -> Result a) -> a -> a
whileSuccess f val
    | isSuccessful (f val) = whileSuccess f (extractSuccessValue (f val))
    | otherwise = val
    
-- 3.b
applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Failure func) _ = Failure func
applyResult (Success func) (Failure val) = Failure val
applyResult (Success func) (Success val) = Success (func val)

-- 3.c
fromResult :: (a -> b) -> (String -> b) -> Result a -> b
fromResult f1 f2 (Success val) = f1 val
fromResult f1 f2 (Failure val) = f2 val

-- 3.d
combineResultsWith :: (a -> b -> c) -> Result a -> Result b -> Result c
combineResultsWith f (Failure val) _ = Failure val
combineResultsWith f (Success val1) (Failure val2) = Failure val2
combineResultsWith f (Success val1) (Success val2) = Success (f val1 val2)

main :: IO ()
main = print (extractFailureValue (combineResultsWith (+) (Failure "x is undefined") (Failure "crash")))