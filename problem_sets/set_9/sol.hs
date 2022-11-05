import CodeWorld

data Iter a
  = Done a
  | Step (Iter a)
  deriving (Show)

-- 1
insert :: Int -> [Int] -> [Int]
insert y [] = [y]
insert y (x:xs)
  | y <= x = y : x : xs
  | otherwise = x : insert y xs

-- 2
approximate :: (a -> Bool) -> (a -> a) -> a -> Iter a
approximate evaluateApprox improveApprox approx = case evaluateApprox approx of
  True -> Done approx
  False -> Step (approximate evaluateApprox improveApprox (improveApprox approx))

-- 3.a
eval :: Iter a -> a
eval (Done val) = val
eval (Step val) = eval val

-- 3.b
limit :: Int -> Iter a -> Iter (Maybe a)
limit 0 _ = Done Nothing
limit _ (Done val) = Done (Just val)
limit n (Step val) = Step (limit (n - 1) val)

-- 3.c
partialEval :: Int -> Iter a -> Iter a
partialEval 0 val = val
partialEval _ (Done val) = Done val
partialEval n (Step val) = partialEval (n - 1) val

-- 3.d
steps :: Iter a -> Int
steps (Done val) = 0
steps (Step val) = 1 + steps val


-- 4.a
mapIter :: (a -> b) -> Iter a -> Iter b
mapIter f (Done val) = Done (f val)
mapIter f (Step val) = Step (mapIter f val)

-- 4.b
joinIter :: Iter (Iter a) -> Iter a
joinIter (Done val) = val
joinIter (Step val) = Step (joinIter val)

-- 5
insertIter :: Int -> [Int] -> Iter [Int]
insertIter y [] = Done [y]
insertIter y numbersList = helper y numbersList []
  where
    helper y [] result = Done (result ++ [y])
    helper y (x:xs) result
      | y <= x = Step (Done (result ++ (y : x : xs)))
      | otherwise = Step (helper y xs (result ++ [x]))

-- 6

addSteps :: Int -> Iter [Int] -> Iter [Int]
addSteps 0 param = param
addSteps n param = Step (addSteps (n-1) param)

insertionSortIter :: [Int] -> Iter [Int]
insertionSortIter numbersList = helper (reverse numbersList) []
  where
    helper [] result = Done result
    helper (x:xs) result = addSteps (steps (insertIter x result)) (helper xs (insert x result))

main :: IO ()
main = print  ( insertionSortIter [4,3..1])