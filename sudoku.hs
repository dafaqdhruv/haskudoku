main :: IO ()
main = return ()
-- import Data.List 

-- Declare types : Sudoku Board, Rows, collums, boxes

type Matrix a = [[a]]
type Board = Matrix Char
type Choices = [Char]

boardSize = 9
boxSize = 3
cellvals = "123456789"
blank e = e == '.'





-- define a row, collumn and boxes
rows :: Matrix a -> Matrix a
rows = id


cols :: Matrix a -> Matrix a
cols [] = []
cols [xs]  =  [[x] | x <-xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs x = map ungroup ( ungroup ( map cols (group  (map group x ))))



-- group and return
group :: [a] -> [[a]]
group xs = groupBy  (floor (sqrt (fromIntegral (length xs)))) xs -- ToDo : Definition of groupBy

ungroup :: [[a]] -> [a]
ungroup = concat

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n xs = as : groupBy n bs
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

