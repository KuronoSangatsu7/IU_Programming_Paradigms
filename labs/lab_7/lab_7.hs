import Text.Read (readMaybe)

type Number = Integer
type State = [Number]

getInt :: IO Int
getInt = do
  putStr "Enter number: "
  input <- getLine
  case readMaybe input of
    Nothing -> do
      putStrLn ("ERROR: invalid input: " ++ input)
      getInt
    Just n -> return n

sumOfTwoInputs :: IO ()
sumOfTwoInputs = do
  x <- getInt
  y <- getInt
  print (x + y)

sumOfManyInputs :: IO ()
sumOfManyInputs = do
  n <- getInt
  xs <- getInts n
  print (sum xs)

getInts :: Int -> IO [Int]
getInts n = sequence'
  (replicate n getInt)

sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (program : programs) = do
  x <- program
  xs <- sequence' programs
  return (x:xs)

main = sumOfManyInputs