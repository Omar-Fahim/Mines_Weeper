type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] deriving (Show,Eq)

seekx :: Int -> [String]
seekx 0 = []
seekx dx | (dx > 0) = "down":(seekx (dx-1))
		| otherwise =  "up":(seekx (dx+1))

seeky :: Int -> [String]
seeky 0 = []		
seeky dy | (dy > 0) = "right":(seeky (dy-1))
		| otherwise =  "left":(seeky (dy+1))

seek :: Cell -> Cell -> [String]
seek (xi,yi) (xf,yf) = (seekx (xf-xi)) ++ (seeky (yf-yi))++["collect"]


solveHelper :: MyState ->[String]-> [String]
solveHelper (S (x,y) []) sol = sol

solveHelper (S (x,y) (h:t)) sol = solveHelper (S h t) (sol ++ (seek (x,y) h))

		
solve :: Cell->[Cell]->[String]

solve pos mines = solveHelper (S pos (sortMines [] pos mines)) []

distToMine :: Cell -> Cell -> Int
distToMine (xi,yi) (xf,yf) = ((abs (xf-xi)) + (abs (yf-yi)))

remMine :: Cell -> [Cell] -> [Cell]

remMine (x,y) [] = []
remMine (x,y) ((hx,hy):t) | (hx == x && hy == y) = t
			| otherwise = (hx,hy):(remMine (x,y) t)

nearestMine :: Cell -> Cell -> Int -> [Cell] -> Cell
			
nearestMine ref near d [] = near

nearestMine ref near d (h:t) | (d == -1) = nearestMine ref h (distToMine ref h) t 
							|  (distToMine ref h) < d = (nearestMine ref h (distToMine ref h) t)
							 | otherwise = nearestMine ref near d t
	
sortMines :: [Cell] -> Cell -> [Cell] -> [Cell]
sortMines sorted (x,y) [] = sorted
sortMines sorted ref mines  = sortMines (sorted ++ [(nearestMine ref (0,0) (-1) mines)]) (nearestMine ref (0,0) (-1) mines) (remMine (nearestMine ref (0,0) (-1) mines) mines)





