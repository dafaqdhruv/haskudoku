import Data.List 
main :: IO ()
main = return ()

-- Declare types : Sudoku Board, Rows, collums, boxes

type Matrix a = [[a]]
type Board = Matrix Char
type Choices = [Char]

boardSize = 9
boxSize = 3
cellvals = "123456789"
blank e = e == '.'

gentle :: Board
gentle = ["2....1.38",
          "........5",
          ".7...6...",
          ".......13",
          ".981..257",
          "31....8..",
          "9..8...2.",
          ".5..69784",
          "4..25...."]
                                                                                                                               


-- define a row, collumn and boxes
rows :: Matrix a -> Matrix a
rows = id


cols :: Matrix a -> Matrix a
cols [] = []
cols [xs]  =  [[x] | x <-xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs x = map ungroup2 ( ungroup2 ( map cols (group2  (map group2 x ))))



-- group2 and return
group2 :: [a] -> [[a]]
group2 xs = group2By  (floor (sqrt (fromIntegral (length xs)))) xs -- ToDo : Definition of group2By

ungroup2 :: [[a]] -> [a]
ungroup2 = concat

group2By :: Int -> [a] -> [[a]]
group2By _ [] = []
group2By n xs = as : group2By n bs
  where (as,bs) = splitAt n xs


-- nodups checks for any duplicates in a given list
nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x : xs) = not (x `elem` xs) && nodups xs


-- correct checks if filled board follows sudoku logic
correct :: Board -> Bool 
correct b = all nodups (rows b) &&  all nodups (cols b) &&  all nodups (boxs b)



-- Function takes input board, returns list of possible solutions
sudoku :: Board -> [Board]
sudoku o = [o]





-- generate choices for blank values
choices :: Board -> Matrix Choices
choices = map (map choose)
choose e = if blank e then cellvals else [e]


-- fixed entries in row/col/box
fixed :: [Choices] -> Choices
fixed = concat.filter single

single :: [a] -> Bool
single xs = length xs == 1

-- remove single, fixed elements
reduce :: [Choices] -> [Choices]
reduce xss = [xs `minus` singles | xs <- xss]
  where singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys 
-- \\ is the list difference operator
-- xs \\ ys removes all the first occurences of each element of ys from xs


-- Now to define a function prune that results in the same answer as previous but, quicker
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where pruneBy f = f . map reduce . f


-- cartesian product
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss)  = [y : ys | y<-xs, ys<-cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp
-- Prune the choies that already occur in row/col/box
sol2  :: Board -> [Board]
sol2 = filter correct . collapse . prune . choices


-- Repeated Pruning
sol3 :: Board -> [Board]
sol3 = filter correct . collapse . fix prune . choices
fix :: Eq a => (a->a) -> a -> a
fix f x = if x == x' then x else fix f x'
  where x' = f x
-- Basically, if further pruning is possible, prune.
-- Even after this though, solution is not optimal.
-- Further evaulation required.





-- PROPeRTIES OF MATRICES
--
-- Completeness
-- A grid is complete if there exists only 1 option for each square
complete :: Matrix Choices -> Bool
complete = all (all single) 

-- A Grid is void if there exists no solution for any square
void :: Matrix Choices -> Bool
void = any (any null)

-- A grid can be called 'safe' when is it is consistent throughout. 
-- This means that no solution is repeated in a row/col/box
safe :: Matrix Choices -> Bool
safe cm = all consistent (rows cm) && all consistent (cols cm) && all consistent (boxs cm)

consistent :: [Choices] -> Bool
consistent = nodups . concat . filter single

-- A grid is blocked if it is void or unsafe
blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)





-- A blocked matrix leads to no solution
-- Blocked matrices are frequent and falling in one is one of the major inefficiencies of our model

sol4  :: Board -> [Board]
sol4 = search . prune . choices

search :: Matrix Choices -> [Board]
search m
  | blocked m  = []
  | complete m = collapse m
  | otherwise  = [g | m' <- expand m
                 , g <- search( prune m')]

-- Expand choices one at a time
expand :: Matrix Choices -> [Matrix Choices]
expand m = 
  [ rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
      (rows1, row : rows2) = span (all  single) m
      (row1,cs : row2) = span single row
