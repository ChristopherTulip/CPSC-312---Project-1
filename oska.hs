import Data.List

-- ****************************************************
-- Data Model
-- ****************************************************

data State = State {
	whites 	 :: [ Point ],
	blacks 	 :: [ Point ],
  board    :: [ Point ],
	turn 		 :: Int
} deriving (Show, Eq, Ord)

data Point = Point {
	x 		   :: Int,
	y 		   :: Int
} deriving (Show, Eq, Ord)

-- ****************************************************
-- Search Level Logic
-- ****************************************************

--minimaxSearch :: State -> Int -> State
---- currentState, depth, return newState

--bestSate :: [ State ] -> State
---- nextStates, return bestState

--sortState state1 state2
--  | (hVal state1) > (hVal state2)   = GT
--  | otherwise                       = LT

---- ****************************************************
---- State Level Logic
---- ****************************************************

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

possibleMoves :: [ Point ] -> State -> [ State ]
possibleMoves pts state
  | null pts                = []
  | otherwise               = merge (validMovesForPoint (head pts) state) (possibleMoves (tail pts) state)

validMovesForPoint :: Point -> State -> [ State ]
validMovesForPoint pt state = merge jumpStates moveStates
  where
    jumpStates = jumpStatesForPoint pt state
    moveStates = moveStateForPoint pt state

moveStateForPoint :: Point -> State -> [ State ]
moveStateForPoint pt state
  | both              = [leftMove, rightMove]
  | leftMove /= state = [leftMove]
  | rightMove/= state = [rightMove]
  | otherwise         = [State [(Point 999 999)] [(Point 999 999)] [(Point 999 999)] white]
  where
    both      = (leftMove /= state) && (rightMove /= state)
    leftMove  = stateAfterMove pt left  state
    rightMove = stateAfterMove pt right state

jumpStatesForPoint :: Point -> State -> [ State ]
jumpStatesForPoint pt state
  | both              = [leftJump, rightJump]
  | leftJump /= state = [leftJump]
  | rightJump/= state = [rightJump]
  | otherwise         = []
  where
    both      = (leftJump /= state) && (rightJump /= state)
    leftJump  = stateAfterJump pt left  state
    rightJump = stateAfterJump pt right state

stateAfterJump :: Point -> Int -> State -> State
stateAfterJump pt dir state
  | whiteJump         = State (whites next) (blacks cleanState) (board state) black
  | blackJump         = State (whites cleanState) (blacks next) (board state) white
  | otherwise         = state
  where
    whiteJump   = (canJump pt dir state) && ( (turn state) == white )
    blackJump   = (canJump pt dir state) && ( (turn state) == black )
    next        = replacePointForState pt ( jump pt dir ( turn state ) ) state
    cleanState  = removePointForState ( move pt dir ( turn state ) ) state

stateAfterMove :: Point -> Int -> State -> State
stateAfterMove pt dir state
  | (canMove pt dir state)  = replacePointForState pt ( move pt dir ( turn state ) ) state
  | otherwise               = state

replacePointForState :: Point -> Point -> State -> State
replacePointForState pt newPt state
  | elem pt ( whites state ) = State (replacePoint pt newPt (whites state)) (blacks state) (board state) (turn state)
  | elem pt ( blacks state ) = State (whites state) (replacePoint pt newPt (blacks state)) (board state) (turn state)
  | otherwise                = state

removePointForState :: Point -> State -> State
removePointForState pt state
  | elem pt ( whites state ) = State (removePoint pt (whites state)) (blacks state) (board state) (turn state)
  | elem pt ( blacks state ) = State (whites state) (removePoint pt (blacks state)) (board state) (turn state)
  | otherwise                = state


removePoint :: Point ->[ Point ] -> [ Point ]
removePoint pt pts
  | (null pts)                    = []
  | pt == (head pts)              = (tail pts)
  | otherwise                     = (head pts) : removePoint pt (tail pts)

replacePoint :: Point -> Point -> [ Point ] -> [ Point ]
replacePoint pt newPt pts
  | (null pts)                    = []
  | pt == (head pts)              = newPt : (tail pts)
  | otherwise                     = (head pts) : (replacePoint pt newPt (tail pts))

-- ****************************************************
-- Point Level Logic (Eg. Moves)
-- ****************************************************

-- currentPoint, currentState, return canJump?
canJump :: Point -> Int -> State -> Bool
canJump pt dir state = bounded && blocked
  where
    nextPoint = jump pt dir (turn state)
    blocked   = pointTaken (move pt dir (turn state)) state
    bounded   = (pointInBounds nextPoint state)

-- currentPoint, currentState, return canJump?
canMove :: Point -> Int -> State -> Bool
canMove pt dir state = bounded && clear
  where
    nextPoint = move pt dir (turn state)
    bounded   = (pointInBounds nextPoint state)
    clear     = not (pointTaken nextPoint state)

pointInBounds :: Point -> State -> Bool
pointInBounds point state = ( elem point (board state) )

pointTaken ::  Point -> State -> Bool
pointTaken point state  = ( elem point (blacks state) ) || ( elem point (whites state) )

-- currentPoint, direction, turn, return movedPoint
move :: Point -> Int -> Int -> Point
move old direction turn
  | turn == white     = ( Point ( (x old) + direction ) ( (y old) + 1 ) )
  | otherwise         = ( Point ( (x old) + direction ) ( (y old) - 1 ) )

-- currentPoint, direction, turn, return movedPoint
jump :: Point -> Int -> Int -> Point
jump old direction turn
  | turn == white     = ( Point ( (x old) + direction ) ( (y old) + 2 ) )
  | otherwise         = ( Point ( (x old) + direction ) ( (y old) - 2 ) )

-- ****************************************************
-- Tests
-- ****************************************************

-- validMovesForPoint :: Point -> State -> [ State ]
t_validMovesForPoint1 = validMovesForPoint (head whiteList) firstState
t_validMovesForPoint2 = validMovesForPoint (nth whiteList 0 1) firstState
t_validMovesForPoint3 = validMovesForPoint (nth whiteList 0 2) firstState
t_validMovesForPoint4 = validMovesForPoint (nth whiteList 0 3) firstState


-- stateAfterMove :: Point -> Int -> State
t_stateAfterMoveWL = stateAfterMove (head (whites blockedStateW)) left  blockedStateW
t_stateAfterMoveBL = stateAfterMove (head (blacks blockedStateB)) left  blockedStateB
t_stateAfterMoveWR = stateAfterMove (head (whites blockedStateW)) right blockedStateW
t_stateAfterMoveBR = stateAfterMove (head (blacks blockedStateB)) right blockedStateB

-- stateAfterJump :: Point -> Int -> State
t_stateAfterJumpWL = stateAfterJump (head (whites blockedStateW)) left  blockedStateW
t_stateAfterJumpBL = stateAfterJump (head (blacks blockedStateB)) left  blockedStateB
t_stateAfterJumpWR = stateAfterJump (head (whites blockedStateW)) right blockedStateW
t_stateAfterJumpBR = stateAfterJump (head (blacks blockedStateB)) right blockedStateB

-- replacePointForState :: Point -> Point -> State -> State
t_replacePointForState = replacePointForState (head whiteList) (Point 10 10) firstState

-- removePointForState :: Point -> State -> State
t_removePointForState = removePointForState (head whiteList) firstState

-- removePoint :: Point ->[ Point ] -> [ Point ]
t_removePointH   = removePoint (head whiteList) whiteList
t_removePointL   = removePoint (last whiteList) whiteList

-- replacePoint :: Point -> Point -> [ Point ] -> [ Point ]
t_replacePointH  = replacePoint (head whiteList) (Point 10 10) whiteList
t_replacePointL  = replacePoint (last whiteList) (Point 10 10) whiteList

-- canMove :: Point -> Int -> State -> Bool
t_canMoveT    = canMove (head (whites firstState)) left firstState
t_canMoveFB   = canMove (head (whites blockedStateW)) right blockedStateW
t_canMoveFOOB = canMove (head (whites blockedStateW)) left blockedStateW

-- canJump :: Point -> Int -> State -> Bool
t_canJumpT    = canJump (head (whites blockedStateW)) right blockedStateW
t_canJumpFOOB = canJump (head (whites blockedStateW)) left blockedStateW
t_canJumpFNB  = canJump (head (whites firstState)) right firstState

-- pointTaken ::  Point -> State -> Bool
t_pointTakenT = pointTaken (Point 4 3) blockedStateW
t_pointTakenF = pointTaken (Point 1 1) blockedStateW

-- pointInBounds :: Point -> State -> Bool
t_pointInBoundsT = pointInBounds (head whiteList) firstState
t_pointInBoundsF = pointInBounds (Point 99 99) firstState

-- move :: Point -> Int -> Int -> Point
t_moveLW = move p1 left  white
t_moveRW = move p1 right white
t_moveLB = move p5 left  black
t_moveRB = move p5 right black

-- jump :: Point -> Int -> Int -> Point
t_jumpLW = jump p1 left  white
t_jumpRW = jump p1 right white
t_jumpLB = jump p5 left  black
t_jumpRB = jump p5 right black

-- Test Structures (Point x y)
p1 = (Point 1 1)
p2 = (Point 2 1)
p3 = (Point 3 1)
p4 = (Point 4 1)

p5 = (Point 1 5)
p6 = (Point 2 5)
p7 = (Point 3 5)
p8 = (Point 4 5)

whiteList = (whites firstState)
blackList = (blacks firstState)

firstState = stringToState testString

blockedStateW = ( State [ (Point 3 2) ] [ (Point 4 3),(Point 3 3) ] testBoard white )
blockedStateB = ( State [ (Point 3 2) ] [ (Point 4 3),(Point 3 3) ] testBoard black )

testBoard = [(Point 3 5),(Point 4 5),(Point 2 5),(Point 1 5),(Point 4 4),(Point 3 4),(Point 2 4),(Point 4 3),(Point 3 3),(Point 4 2),(Point 3 2),(Point 2 2),(Point 4 1),(Point 3 1),(Point 2 1),(Point 1 1)]


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
merge [] []               = []
merge xs []               = xs
merge [] ys               = ys
merge (x:xs) (y:ys)       = x : y : merge xs ys

nth :: [a] -> Int -> Int -> a
nth arr i depth
  | i == depth  = (head arr)
  | otherwise   = nth (tail arr) (i+1) depth

--
-- Constants
--
white = 0
black = 1

left 	= 1
right = 0
