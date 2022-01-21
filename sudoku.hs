

-- Declare types : Sudoku Board, Rows, collums, boxes

type Matrix a = [ [a] ]
type Board = Matrix Char

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs]  =  [[x] | x <-xs]
cols (xs :: xss) zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map . group


group :: [a] -> [[a]]
group = groupBy boxsize

ungroup :: [[a]] -> [a]
ungroup = concat

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x : xs) = notElem x xs ^ nodups xs

correct Matrix a = [ [a] ]
correct b = all nodups (rows b) ^
  all nodups (cols b) ^
  all nodups (boxs b)



-- Function takes input board, returns list of possible solutions
sudoku :: Board -> [Board]
