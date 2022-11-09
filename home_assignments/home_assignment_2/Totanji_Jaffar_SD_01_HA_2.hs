import CodeWorld

-- Author: Jaffar Totanji - SD - 01

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

-- A function to shift the focus on a given line by a given number of positions to the left (negative values) or to the right (positive values)
shiftRepeated :: Line a -> Integer -> a -> Line a
shiftRepeated line 0 _ = line
shiftRepeated line n _
  | n > 0 = shiftRightRepeated n line
  | otherwise = shiftLeftRepeated (abs n) line

-- A function which maps every cell in a line into a version of the original line where that cell is in focus. The new line of lines has the original line in focus
lineShifts :: Line a -> Line (Line a)
lineShifts line = zipLinesWith (shiftRepeated line) integers line

-- A function that applies Rule 30 to every element of a given line of cells
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- 1.8

-- A function which renders an array of Pictures shifting each consecutive Picture one position to the left
renderLeft :: [Picture] -> Picture
renderLeft [] = blank
renderLeft (x:xs) = x <> translated (-1) 0 (renderLeft xs)

-- A function which renders an array of Pictures shifting each consecutive Picture one position to the right
renderRight :: [Picture] -> Picture
renderRight [] = blank
renderRight (x:xs) = x <> translated 1 0 (renderRight xs)

-- A function that renders a Line of Pictures with proper translations
renderLine :: Line Picture -> Picture
renderLine (Line xs y zs) = translated (-1) 0 (renderLeft xs) <> y <> translated 1 0 (renderRight zs)


-- A function that translates an Alive Cell to a solid rectangle and a Dead Cell to a hollow one
cellToPicture :: Cell -> Picture
cellToPicture Alive = solidRectangle 1 1
cellToPicture _ = rectangle 1 1

-- A function that renders the fist N steps of Rule 30 applied to a given starting line
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 0 line = renderLine (mapLine cellToPicture line)
renderRule30 n line = renderRule30 (n - 1) newLine
  where
    newLine = applyRule30 line

-- Sample lines
sampleLine1 :: Line Cell
sampleLine1 = Line (repeat Dead) Alive (repeat Dead)

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

-- #1.3 Discrete Spaces

data Space a = Space (Line (Line a))

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

-- 1.11 DUPLICATE

-- #1.4 Conway’s Game of Life

-- 1.12

-- A function that computes the state of a cell given its current state and the number of alive neighbors it has according to the rules of Conway's Game of Life
computeConwayRule :: Cell -> Integer -> Cell
computeConwayRule Alive 2 = Alive
computeConwayRule _ 3 = Alive
computeConwayRule _ _ = Dead

-- A function that returns the focus of a given line of abstract items
getFocus :: Line a -> a
getFocus (Line xs y zs) = y

-- A function that returns the focus of a given space
getSpaceFocus :: Space Cell -> Cell
getSpaceFocus (Space (Line xs y zs)) = getFocus y

-- A function that returns the first cell prior to the focus of a given line
getPrevLineFocus :: Line Cell -> Cell
getPrevLineFocus (Line [] y zs) = Dead
getPrevLineFocus (Line (x:xs) y zs) = x

-- A function that returns the first cell after the focus of a given line
getNextLineFocus :: Line Cell -> Cell
getNextLineFocus (Line xs y []) = Dead
getNextLineFocus (Line xs y (z:zs)) = z

-- A function that returns the first line prior to the focus of a given space
getPrevSpaceFocus :: Line (Line Cell) -> Line Cell
getPrevSpaceFocus (Line [] y zs) = (Line [Dead] Dead [Dead])
getPrevSpaceFocus (Line (x:xs) y zs) = x

-- A function that returns the first line after the focus of a given space
getNextSpaceFocus :: Line (Line Cell) -> Line Cell
getNextSpaceFocus (Line xs y []) = (Line [Dead] Dead [Dead])
getNextSpaceFocus (Line xs y (z:zs)) = z

-- A function that returns the focus of a given line and the 2 cells to its left and right
getThreeCells :: Line Cell -> [Cell]
getThreeCells line = [(getPrevLineFocus line)] ++ [(getFocus line)] ++ [(getNextLineFocus line)]

-- A function that returns an array containing the Moore neighborhood of the cell in focus of a given space
getMoore :: Space Cell -> [Cell]
getMoore (Space line) = (getThreeCells (getPrevSpaceFocus line)) ++ [(getPrevLineFocus (getFocus line))] ++ [(getNextLineFocus (getFocus line))] ++ (getThreeCells (getNextSpaceFocus line))

-- A function that returns the number of alive neighbors of the cell in focus of a given space
aliveNeighbors :: Space Cell -> Integer
aliveNeighbors space = sum (map (cellToBinary) (getMoore space))

-- A function that computes the next state of the cell in focus according to the rules of Conway’s Game of Life
conwayRule :: Space Cell -> Cell
conwayRule space = computeConwayRule (getSpaceFocus space) (aliveNeighbors space)

-- 1.13

-- A function that performs a shift operation on a line and returns a new, shifted line if the operation is a success and the original otherwise
pureShift :: (Line a -> Maybe (Line a)) -> Line a -> Line a
pureShift f line = case f line of
  Nothing -> line
  Just result -> result

-- A function that takes a space and shift its focus up by one position
shiftSpaceUp :: Space a -> Maybe (Space a)
shiftSpaceUp (Space (Line [] y zs)) = Nothing
shiftSpaceUp (Space line) = Just (Space (pureShift shiftLeft line))

-- A function that takes a space and shift its focus down by one position
shiftSpaceDown :: Space a -> Maybe (Space a)
shiftSpaceDown (Space (Line xs y [])) = Nothing
shiftSpaceDown (Space line) = Just (Space (pureShift shiftRight line))

-- A function that takes a space and shift its focus left by one position
shiftSpaceLeft :: Space a -> Maybe (Space a)
shiftSpaceLeft (Space (Line _ (Line [] y zs) _)) = Nothing
shiftSpaceLeft (Space line) = Just (Space (mapLine (pureShift shiftLeft) line))

-- A function that takes a space and shift its focus right by one position
shiftSpaceRight :: Space a -> Maybe (Space a)
shiftSpaceRight (Space (Line _ (Line xs y []) _)) = Nothing
shiftSpaceRight (Space line) = Just (Space (mapLine (pureShift shiftRight) line))

-- A function that takes a space and returns a list of all possible left shifts of that space
shiftSpaceLeftAll :: Space a -> [Space a]
shiftSpaceLeftAll space = case shiftSpaceLeft space of
  Nothing -> []
  Just newSpace -> newSpace : shiftSpaceLeftAll newSpace

-- A function that takes a space and returns a list of all possible right shifts of that space
shiftSpaceRightAll :: Space a -> [Space a]
shiftSpaceRightAll space = case shiftSpaceRight space of
  Nothing -> []
  Just newSpace -> newSpace : shiftSpaceRightAll newSpace

-- A function that takes a space and returns a line of all possible horizontal shifts of that space, with the original space in focus
horizontalSpaceShifts :: Space a -> Line (Space a)
horizontalSpaceShifts space = Line (shiftSpaceLeftAll space) space (shiftSpaceRightAll space)

-- A function that takes a space and returns a list of lines representing all possible upwards shifts of that space
-- Each element of the line also contains all possible horizontal shifts of the original space
shiftSpaceUpAll :: Space a -> [Line (Space a)]
shiftSpaceUpAll space = case (shiftSpaceUp space) of
  Nothing -> [(horizontalSpaceShifts space)]
  Just newSpace -> (horizontalSpaceShifts space) : shiftSpaceUpAll newSpace

-- A function that takes a space and returns a list of lines representing all possible downwards shifts of that space
-- Each element of the line also contains all possible horizontal shifts of the original space
shiftSpaceDownAll :: Space a -> [Line (Space a)]
shiftSpaceDownAll space = case (shiftSpaceDown space) of
  Nothing -> [horizontalSpaceShifts space]
  Just newSpace -> (horizontalSpaceShifts space) : shiftSpaceDownAll newSpace

-- A function that takes a space and converts each cell of that space into a version of the original space with focus shifted to that cell 
-- The new space (of spaces) has the original space in focus
spaceShifts :: Space a -> Space (Space a)
spaceShifts space = (Space (Line (shiftSpaceUpAll (iteratedUp)) (horizontalSpaceShifts space) (shiftSpaceDownAll (iteratedDown))))
  where
    iteratedUp = case shiftSpaceUp space of
      Nothing -> space
      Just newSpace -> newSpace
      
    iteratedDown = case shiftSpaceDown space of
      Nothing -> space
      Just newSpace -> newSpace

-- A function that applies Conway's Rule to every Space in a Space of Spaces
applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)

-- 1.14

-- A function which renders an array of Lines of Pictures shifting each consecutive Line one position upwards
renderAbove :: [Line Picture] -> Picture
renderAbove [] = blank
renderAbove (x:xs) = renderLine x <> translated 0 (-1) (renderAbove xs)

-- A function which renders an array of Lines of Pictures shifting each consecutive Line one position downwards
renderBelow :: [Line Picture] -> Picture
renderBelow [] = blank
renderBelow (x:xs) = renderLine x <> translated 0 1 (renderBelow xs)

-- A function that renders a space of 1x1 pictures
renderSpace :: Space Picture -> Picture
renderSpace (Space (Line xs y zs)) = translated 0 (-1) (renderAbove xs) <> renderLine y <> translated 0 1 (renderBelow zs)

-- A visualizer function to be used by activityOf. It turns a state of (time, space) pair into a picture to be displayed
visualize :: (Double, Space Cell) -> Picture
visualize (_, space) = renderSpace (mapSpace cellToPicture space)

-- An even handling function to be used by activityOf. It updates the state of (time, space) pair given an event
eventHandler :: Event -> (Double, Space Cell) -> (Double, Space Cell)
eventHandler (TimePassing delta) (time, space)
      | time >= 1 = (0, (applyConwayRule space))
      |  otherwise = (time + delta, space)
eventHandler _ world = world

-- A function to Animate Conway's Game of Life starting with a given space and updating it every second
animateConway :: Space Cell -> IO ()
animateConway space = activityOf initialSpace eventHandler visualize
  where
    initialSpace = (0, space)

-- Some sample spaces (some were written by classmates)

blinker = (Space (Line blinker' (Line [Alive, Dead, Dead] Alive [Alive, Dead, Dead]) blinker')) 
  where
    blinker' = replicate 3 (Line (replicate 3 Dead) Dead (replicate 3 Dead))

integerSpace = (Space (Line integerSpaceList integerSpaceAtom integerSpaceList))
  where
    integerSpaceAtom = cutLine 5 integers
    integerSpaceList = take 2 (repeat integerSpaceAtom)

integerSpaceMini = (Space (Line integerSpaceList integerSpaceAtom integerSpaceList))
  where
    integerSpaceAtom = cutLine 1 integers
    integerSpaceList = take 2 (repeat integerSpaceAtom)

pentaDecathlon :: Space Cell 
pentaDecathlon = Space(
  Line 
      [
          Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
          Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
          Line [Alive, Dead, Dead, Dead, Dead] Dead  [Alive, Dead, Dead, Dead, Dead],
          Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
      ]
      (   Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead])
      [
          Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
          Line [Alive, Dead, Dead, Dead, Dead] Dead  [Alive, Dead, Dead, Dead, Dead],
          Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
          Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
      ])

glider :: Space Cell
glider =  Space(
    Line 
        [
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
        ]
        (   Line [Alive, Dead, Dead, Dead, Dead] Alive [Alive, Dead, Dead, Dead, Dead])
        [
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Alive, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Alive [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead],
            Line [Dead, Dead, Dead, Dead, Dead] Dead [Dead, Dead, Dead, Dead, Dead]
        ])

main :: IO()
main = do
animateConway pentaDecathlon
