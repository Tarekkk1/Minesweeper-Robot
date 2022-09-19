type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

-- maxx x y z r    | (x>=y && x >= z && x>= r) = x 
--                 | (y>=x && y>=z && y>=r) = y
--                 | (z>=x && z>=y && z>=r )= z
--                 | otherwise = r
-- if t == [] then maxx x y m n else maxx m n (getGrid (x, y) t) 

maxx maxSoFar [] = maxSoFar
maxx maxSoFar ((_, x):t)| x > maxSoFar = maxx x t
                        | otherwise = maxx maxSoFar t

maxy maxSoFar [] = maxSoFar
maxy maxSoFar ((y, _):t)| y > maxSoFar = maxx y t
                        | otherwise = maxy maxSoFar t

-- getGrid:: Cell -> [Cell] -> Cell
-- getGrid (y, x)  mines = ((maxy y mines), (maxx x mines)) 

insertion (rx,ry) (x,y) []= [(x,y)]
insertion (rx,ry) (x,y) ((x1,y1):t) = if  (abs (x-rx)+abs (y-ry))<(abs (x1-rx)+abs (y1-ry)) then (x,y):(x1,y1):t else (x1,y1): (insertion (rx,ry) (x,y) t) 
sortMines _ [] = []
sortMines (rx,ry) (x:xs)= insertion (rx,ry) x (sortMines (rx,ry) xs)
-- Takes a state and returns state after moving up in grid
-- if out of bounds return null

up:: MyState -> MyState
up (S (ry, rx) minePos lastAct p)           | (ry - 1 < 0) = Null
                                            | otherwise = (S (ry - 1, rx) minePos "up" (S (ry, rx) minePos lastAct p))

-- Takes a state and returns state after moving down in grid
-- if out of bounds return null
down:: MyState -> MyState
down (S (ry, rx) minePos lastAct p)    | (ry + 1 > (maxy ry minePos)) = Null
                                                | otherwise = (S (ry + 1, rx) minePos "down" (S (ry, rx) minePos lastAct p))

-- Takes a state and returns state after moving left in grid
-- if out of bounds return null
left:: MyState -> MyState
left (S (ry, rx) minePos lastAct p)    | (rx - 1 < 0) = Null
                                                | otherwise = (S (ry, rx - 1) minePos "left" (S (ry, rx) minePos lastAct p))

-- Takes a state and returns state after moving right in grid
-- if out of bounds return null
right:: MyState -> MyState
right (S (ry, rx) minePos lastAct p)   | (rx + 1 > (maxx rx minePos)) = Null 
                                                | otherwise = (S (ry, rx + 1) minePos "right" (S (ry, rx) minePos lastAct p))

-- Recursively checks through all mine positions removing those that the robot is on
collect:: MyState -> MyState
collect (S robotPos minePos lastAct p)  | (collectHelper robotPos minePos) == minePos = Null
                                        | otherwise = (S robotPos (collectHelper robotPos minePos) "collect" (S robotPos minePos lastAct p))
-- Helper that takes robot position and all remaining mine positions, returns mine positions after collection
collectHelper _ [] = []
collectHelper robotPos (minePos:t)  | robotPos == minePos = sortMines robotPos t
                                    | otherwise = minePos : (collectHelper robotPos t)

-- Returns list of next possible states, that do not result in Null
-- up, down, left, right, and collect
nextMyStates::MyState -> MyState
nextMyStates (S (ry, rx) ((y, x):t) lastAct p)  | (ry < y) = down (S (ry, rx) ((y, x):t) lastAct p)
                                                | (ry > y) = up (S (ry, rx) ((y, x):t) lastAct p)
                                                | (rx < x) = right (S (ry, rx) ((y, x):t) lastAct p)
                                                | (rx > x) = left (S (ry, rx) ((y, x):t) lastAct p)
                                                | otherwise = collect (S (ry, rx) ((y, x):t) lastAct p)


isGoal:: MyState -> Bool
isGoal (S robotPos minePos lastAct p) = (minePos == [])

search::MyState->MyState
search h    | (isGoal h) = h
            | otherwise = search (nextMyStates h)

constructSolution:: MyState ->[String]
constructSolution (S robotPos minePos lastAct p)| lastAct == "" = []
                                                | otherwise = (constructSolution p) ++ [lastAct]

solve :: Cell -> [Cell] ->[String]
solve robotPos minePos = constructSolution(search (nextMyStates(S robotPos (sortMines robotPos minePos) "" Null)))