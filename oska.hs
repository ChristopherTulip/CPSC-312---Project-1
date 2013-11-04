import Data.List

-- ****************************************************
-- Data Model
-- ****************************************************

data State = State {
	whitePieces 	:: [ Point ],
	blackPieces 	:: [ Point ],
  board         :: [ Point ],
	turn 		      :: Int
} deriving (Show, Eq, Ord)

data Point = Point {
	x 		:: Int,
	y 		:: Int
} deriving (Show, Eq, Ord)

-- ****************************************************
-- Search Level Logic
-- ****************************************************

--minimaxSearch :: State -> Int -> State
---- currentState, depth, return newState

--bestSate :: [ State ] -> State
---- nextStates, return bestState

----
---- State Level Logic
----

--movesForState :: State -> [ State ]
---- currentState, return possibleStates
----
---- for each Point get nextStates
----

--getHeuristicValue :: State -> Int
---- currentState, return value

-- ****************************************************
-- Point Interactions
-- ****************************************************

--trimInvalidMoves :: [ Point ] -> State -> [ Point ]
--trimInvalidMoves pts state
--  | (validMove (head pts) state)  =
--  | otherwise                     =

possibleMoves :: Point -> State -> [ Point ]
possibleMoves pt state = [ jumpLeft, jumpRight, moveLeft, moveRight ]
  where
    jumpLeft  = jump pt left (turn state)
    jumpRight = jump pt right (turn state)
    moveLeft  = jump pt left (turn state)
    moveRight = jump pt right (turn state)


--removePoint :: Point -> State -> State
---- PointToRemove, currentState, return newState

--replacePoint :: Point -> Point -> State -> State
---- PointToReplace, replacement, currentState, return newState

-- currentPoint, currentState, return canJump?
canJump :: Point -> Int -> State -> Bool
canJump pt dir state = bounded && blocked
  where
    nextPoint = jump pt dir (turn state)
    blocked   = pointBlocked (move pt dir (turn state)) state
    bounded   = (pointInBounds nextPoint state)

-- currentPoint, currentState, return canJump?
canMove :: Point -> Int -> State -> Bool
canMove pt dir state = bounded && clear
  where
    nextPoint = move pt dir (turn state)
    bounded   = (pointInBounds nextPoint state)
    clear     = not (pointBlocked nextPoint state)

pointInBounds :: Point -> State -> Bool
pointInBounds point state = ( elem point (board state) )

pointBlocked ::  Point -> State -> Bool
pointBlocked point state
  | (turn state) == white = ( elem point (blackPieces state) )
  | otherwise             = ( elem point (whitePieces state) )

-- ****************************************************
-- Point Level Logic (Eg. Moves)
-- ****************************************************

-- currentPoint, direction, turn, return movedPoint
move :: Point -> Int -> Int -> Point
move old direction turn
  | turn == white     = ( Point ( (x old) + direction ) ( (y old) + 1 ) )
  | otherwise         = ( Point ( (x old) + direction ) ( (y old) - 1 ) )

-- currentPoint, direction, turn, return movedPoint
jump :: Point -> Int -> Int -> Point
jump old direction turn
  | turn == white     = ( Point ( (x old) + (2 * direction) ) ( (y old) + 2 ) )
  | otherwise         = ( Point ( (x old) + (2 * direction) ) ( (y old) - 2 ) )

-- ****************************************************
-- Tests
-- ****************************************************

-- Can Jump
canJumpT    = canJump (head (whitePieces blockedState)) right blockedState
canJumpOOBF = canJump (head (whitePieces blockedState)) left blockedState
canJumpF    = canJump (head (whitePieces firstState)) left firstState

-- pointInBounds
pointInBoundsT = pointInBounds (head whiteList) firstState
pointInBoundsF = pointInBounds (Point 99 99) firstState

-- Move
moveLW = move p1 left white
moveRW = move p1 right white
moveLB = move p5 left black
moveRB = move p5 right black

-- Jump
jumpLW = jump p1 left white
jumpRW = jump p1 right white
jumpLB = jump p5 left black
jumpRB = jump p5 right black

-- Test Structures (Point x y)
p1 = (Point 1 1)
p2 = (Point 2 1)
p3 = (Point 3 1)
p4 = (Point 4 1)

p5 = (Point 1 5)
p6 = (Point 2 5)
p7 = (Point 3 5)
p8 = (Point 4 5)

whiteList = whitePieces firstState
blackList = blackPieces firstState

firstState = stringToState testString
--
-- X X X
--  X X
-- X X X
--
blockedState = ( State [ (Point 1 1) ] [ (Point 1 2), (Point 2 2) ] [ (Point 1 1), (Point 1 2), (Point 1 3), (Point 2 1), (Point 2 2), (Point 1 3), (Point 2 3), (Point 3 3) ] white )

testString = [ "wwww", "---", "--",  "---", "bbbb" ]

-- ****************************************************
-- Process Input Signal - Assumes only initial states
-- ****************************************************
stringToState :: [String] -> State
stringToState input = ( State whites blacks board white )
  where
    whites = (buildWhites (head input) )
    blacks = (buildBlacks (last input) (length input) )
    board  = (buildBoard input ( length (head input) ) (length input) )

buildWhites :: String -> [ Point ]
buildWhites input
  | null input        = []
  | otherwise         = (Point (length input) 1) : buildWhites (tail input)


buildBlacks :: String -> Int -> [ Point ]
buildBlacks input height
  | null input        = []
  | otherwise         = (Point (length input) height) : (buildBlacks (tail input) height)

buildBoard :: [ String ] -> Int -> Int -> [ Point ]
buildBoard input maxWidth height
  | null input        = []
  | otherwise         = merge ( processLine (head input) (length input) offset ) next
  where
    next      = ( buildBoard (tail input) maxWidth height )
    offset    = ( maxWidth - (length (head input) ) )

-- length         4 3 2 3 4
-- start indicies 1 2 3 2 1

processLine :: String -> Int -> Int -> [ Point ]
processLine line yCoord offset
  | null line           = []
  | otherwise           = pt : next
  where
    pt    = ( Point xCoord yCoord )
    next  = ( processLine (tail line) yCoord offset )
    xCoord= ( offset + (length line) )

-- ****************************************************
-- Helpers
-- ****************************************************
merge :: [a] -> [a] -> [a]
merge xs []               = xs
merge [] ys               = ys
merge (x:xs) (y:ys)       = x : y : merge xs ys

--
-- Constants
--
white = 0
black = 1

left 	= (-1)
right = 1
