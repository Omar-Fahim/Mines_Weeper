type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)


up:: MyState -> MyState

up (S (x,y) l s o ) | (x > 0) = S (x-1,y) l "up" (S (x,y) l s o)
                | otherwise = Null

down:: MyState -> MyState

down (S (x,y) l s o ) | (x < 3) = S (x+1,y) l "down" (S (x,y) l s o)
	| otherwise = Null

left:: MyState -> MyState

left (S (x,y) l s o )| (y>0) = S (x,y-1) l "left" (S (x,y) l s o)
		| otherwise = Null

right:: MyState -> MyState

right (S (x,y) l s o ) | (y<3) = S (x,y+1) l "right" (S (x,y) l s o)
 		| otherwise = Null

collect:: MyState -> MyState

collect (S (x,y) l s o ) | (elem (x,y) l ) = S (x,y) (remMine (x,y) l) "collect" (S (x,y) l s o)
		| otherwise = Null


remMine :: Cell -> [Cell] -> [Cell]

remMine (x,y) [] = []
remMine (x,y) ((hx,hy):t) | (hx == x && hy == y) = t
			| otherwise = (hx,hy):(remMine (x,y) t)

nextMyStates::MyState -> [MyState]

nextMyStates (S (x,y) l s o) = remNulls (nextHelper (S (x,y) l s o))

nextHelper:: MyState -> [MyState]

nextHelper (S (x,y) l s o) = up (S (x,y) l s o):down (S (x,y) l s o):left (S (x,y) l s o):right (S (x,y) l s o):collect(S (x,y) l s o):[]

remNulls :: [MyState] -> [MyState]

remNulls [] = []


remNulls (x:xs)  | x == Null = remNulls xs
                  |otherwise = x:remNulls xs

isGoal::MyState->Bool

isGoal (S (x,y) l s o) = length l == 0

search::[MyState]->MyState

search (x:xs)  | isGoal x = x
               | otherwise =  search(xs ++ nextMyStates x)
				


constructSolution :: MyState ->[String]
constructSolution (S (x,y) l "" o) = []

constructSolution (S (x,y) l s o) = (constructSolution o)++[s]

			
solve :: Cell->[Cell]->[String]

solve pos mines = constructSolution (search [S pos mines "" Null])

		
			
			