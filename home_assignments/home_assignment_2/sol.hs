import CodeWorld

-- #1.1 Lines

data Line a = Line [a] a [a]
  deriving (Show)

-- Example
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- 1.1

-- A function that takes an array and keeps up to a given number of elements in it
cutArray :: Int -> [a] -> [a]
cutArray 0 arr = []
cutArray len [] = []
cutArray len (x:xs)
  | len < 0 = x:xs
  | otherwise = x : cutArray (len - 1) xs

-- A function that keeps up to a given number of elements in each direction in a line
cutLine :: Int -> Line a -> Line a
cutLine len (Line xs y zs) = Line (cutArray len xs) y (cutArray len zs)

-- 1.2

-- A function that applies a given function to a given element repeatedly to produce an array
genArray :: (a -> Maybe a) -> a -> [a]
genArray f x = result
  where
    result = case f x of
      Just a -> a : genArray f a
      Nothing -> []

-- A function that generates a line with x in its focus by using generating functions
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (genArray f x) x (genArray g x)

-- 1.3

-- A function that applies a given function to all elements on a line to produce a new line
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line xs y zs) = Line (map f xs) (f y) (map f zs)

-- 1.4

-- A function that zips together two lines
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line xs y zs) (Line as b cs) = Line (zip xs as) (y, b) (zip zs cs)

-- A function that takes an atom in the form of a pair and applies a given function to it to produce a single result
combineAtomWith :: (a -> b -> c) -> (a, b) -> c
combineAtomWith f element = (f (fst element) (snd element))

-- A function that applies a given combining function to a given array of pairs
combineArrayWith :: (a -> b -> c) -> [(a, b)] -> [c]
combineArrayWith f [] = []
combineArrayWith f (x:xs) = combineAtomWith f x : combineArrayWith f xs

-- A function that applies a given combining function to a given line which was a result of a previous zipLines operation
combineLineWith :: (a -> b -> c) -> Line (a, b) -> Line c
combineLineWith f (Line xs y zs) = Line (combineArrayWith f xs) (combineAtomWith f y) (combineArrayWith f zs)

-- A function that zips together two lines with a given combining function
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f firstLine secondLine = combineLineWith f (zipLines firstLine secondLine)

-- #1.2 Rule 30

-- 1.5

data Cell = Alive | Dead
  deriving (Show)

-- A function that maps the value of a given cell to 1 for Alive and 0 for Dead
cellToBinary :: Cell -> Integer
cellToBinary Alive = 1
cellToBinary _ = 0

-- A Bitwise NOT operator
_not :: Integer -> Integer
_not 0 = 1
_not _ = 0

-- A Bitwise OR operator
_or :: Integer -> Integer -> Integer
_or 1 _ = 1
_or 0 x = x

-- A Bitwise XOR operator
_xor :: Integer -> Integer -> Integer
_xor 0 x = x
_xor 1 x = _not x


-- A function that computes the next state of a cell given the current state of the cell and both its neighbours
computeRule30 :: Cell -> Cell -> Cell -> Cell
computeRule30 x y z = result
  where
    result = case (_xor (cellToBinary x) (_or (cellToBinary y) (cellToBinary z))) of
      1 -> Alive
      0 -> Dead

-- A function to compute the next state of the cell in focus according to Rule 30
rule30 :: Line Cell -> Cell
rule30 (Line [] y (z:_)) = computeRule30 Dead y z
rule30 (Line (x:_) y []) = computeRule30 x y Dead
rule30 (Line [] y []) = computeRule30 Dead y Dead
rule30 (Line (x:_) y (z:_)) = computeRule30 x y z

-- 1.6

-- A function to shift the focus on a given line by one position to the left
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] y zs) = Nothing
shiftLeft (Line (x:xs) y zs) = Just (Line xs x ([y] ++ zs))

-- A function to shift the focus on a given line by one position to the right
shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line xs y []) = Nothing
shiftRight (Line xs y (z:zs)) = Just (Line ([y] ++ xs) z zs)

-- 1.7

-- A function to shift the focus on a given line by a given number of positions to the left
shiftLeftRepeated :: Integer -> Line a -> Line a
shiftLeftRepeated 0 line = line
shiftLeftRepeated n line = shiftLeftRepeated (n - 1) result
  where
    result = case shiftLeft line of
      Nothing -> line
      Just line -> line

-- A function to shift the focus on a given line by a given number of positions to the right
shiftRightRepeated :: Integer -> Line a -> Line a
shiftRightRepeated 0 line = line
shiftRightRepeated n line = shiftRightRepeated (n - 1) result
  where
    result = case shiftRight line of
      Nothing -> line
      Just line -> line

-- A function to shift the focus on a given line by a given number of positions to the left (negative values) or right (positive values)
shiftRepeated :: Line a -> Integer -> a -> Line a
shiftRepeated line 0 element = line
shiftRepeated line n element
  | n > 0 = shiftRightRepeated n line
  | otherwise = shiftLeftRepeated (abs n) line


-- A function which maps every cell in a line into a version of the original line where that cell is in focus. The new line of lines has the original line in focus
lineShifts :: Line a -> Line (Line a)
lineShifts line = combineLineWith (shiftRepeated line) (zipLines integers line) 

-- A function that applies Rule 30 to every element of a given line of cells
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- 1.8

-- A function that generates a picture given a list of pictures. The function starts rendering with offset 0 and increments it by a given value at every element
renderList :: [Picture] -> Double -> Picture
renderList list offset = helper list offset 1
  where
    helper [] _ _ = blank
    helper (x:xs) offset mul = (translated (offset * mul) 0 x) <> helper xs offset (mul + 1)

-- A function that renders a line of 1x1 Pictures
renderLine :: Line Picture -> Picture
renderLine (Line xs y zs) = renderList xs (-1) <> y <> renderList zs 1

-- For testing renderLine
sampleLine :: Line Picture
sampleLine = Line [a,b,c,d,e,f,g] c [g,d,b,c,a,f]
  where
    a = colored red (solidCircle 0.5)
    b = colored green (solidCircle 0.5)
    c = colored blue (solidCircle 0.5)
    d = colored yellow (solidCircle 0.5)
    e = colored purple (solidCircle 0.5)
    f = colored brown (solidCircle 0.5)
    g = colored pink (solidCircle 0.5)

-- A function that translates an Alive Cell to a solid rectangle and a Dead Cell to a hollow one
cellToPicture :: Cell -> Picture
cellToPicture Alive = solidRectangle 1 1
cellToPicture _ = rectangle 1 1

-- For testing
sampleLine1 :: Line Cell
sampleLine1 = Line (repeat Dead) Alive (repeat Dead)

-- A function that renders the fist N steps of Rule 30 applied to a given starting line
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 0 line = renderLine (mapLine cellToPicture line)
renderRule30 n line = renderRule30 (n - 1) newLine
  where
    newLine = applyRule30 line

-- #1.3 Discrete Spaces

data Space a = Space (Line (Line a))
  deriving Show

-- 1.9
-- None

-- 1.10

-- A function that applies a given function to all elements in a Space to produce a new Space
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace f (Space line) = Space (mapLine (mapLine f) line)

-- A function that zips together 2 Spaces
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space line1) (Space line2) = Space (zipLinesWith (zipLines) line1 line2)

-- A function that applies a given combining function to a given Space which was a result of a previous zipSpaces operation
combineSpaceWith :: (a -> b -> c) -> Space (a, b) -> Space c
combineSpaceWith f (Space line) = Space (mapLine (combineLineWith f) line)

-- A function that zips together two Spaces with a given combining function
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith f firstSpace secondSpace = combineSpaceWith f (zipSpaces firstSpace secondSpace)

-- An example space
blinker = (Space (Line blinker' (Line [Alive, Dead] Alive [Alive, Dead]) blinker')) 
  where
    blinker' = replicate 2 (Line (replicate 2 Dead) Dead (replicate 2 Dead))

-- Another example space
integerSpace = (Space (Line integerSpaceList integerSpaceAtom integerSpaceList))
  where
    integerSpaceAtom = cutLine 5 integers
    integerSpaceList = take 2 (repeat integerSpaceAtom)

-- 1.11 DUPLICATE

-- #1.4 Conway’s Game of Life

-- 1.12



main :: IO()
main = print (zipSpacesWith (*) integerSpace integerSpace)