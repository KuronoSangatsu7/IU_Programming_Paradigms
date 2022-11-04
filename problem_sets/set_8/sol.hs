import CodeWorld

-- 1.a
isSingleton :: [a] -> Bool
isSingleton (x:[]) = True
isSingleton _ = False

-- 1.b
insert :: Int -> [Int] -> [Int]
insert y [] = [y]
insert y (x:[])
  | y <= x = y : [x]
  | otherwise = x : [y]
insert y (x:xs)
  | y <= x = y : x : xs
  | otherwise = x : insert y xs
  
-- 1.c
separateBy :: a -> [a] -> [a]
separateBy _ [] = []
separateBy _ (x : []) = [x]
separateBy delim (x : xs) = x : delim : separateBy delim xs

-- 1.d
splitWhenNot :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenNot f list = (takeWhile f list, dropWhile f list)

-- 1.e
groupsSeparatedBy :: (a -> Bool) -> [a] -> [[a]]
groupsSeparatedBy _ [] = []
groupsSeparatedBy f list = takeWhile (\x -> not (f x)) list : groupsSeparatedBy f list2
  where
    list2 = case dropWhile (\x -> not (f x)) list of
      [] -> []
      (x : xs) -> xs
    
-- 1.f
replicateWithPos :: [a] -> [a]
replicateWithPos list = helper 1 list
  where
    helper pos (x : []) = (replicate pos x)
    helper pos (x : xs) = (replicate pos x) ++ helper (pos + 1) xs

-- 2.a
lucas :: [Int]
lucas = lucasize [2,1]
lucasize (x1 : x2 : xs) = x1 : lucasize (x2 : [x1 + x2])

-- 2.b
approximationsOfRoot2 :: Double -> [Double]
approximationsOfRoot2 x = improve x

improve x = x : improve (x - x/2 + 1/x)

main :: IO ()
main = print (take 5 (approximationsOfRoot2 1))