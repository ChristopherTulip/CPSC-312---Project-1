-- Project 1
-- Chris Tulip 24047094 v4l7
-- Julia Litke 27807098 t1v7


import Data.List

-- ****************************************************
-- Data Model
-- ****************************************************

data State = State {
	whites 	 :: [ Point ],
	blacks 	 :: [ Point ],
    board    :: [ Point ],
	turn 	 :: Int,
	value	 :: Int
} deriving (Show, Eq, Ord)

data Point = Point {
	x 		   :: Int,
	y 		   :: Int
} deriving (Show, Eq, Ord)

-- ****************************************************
-- Constants (Don't mess with these)
-- ****************************************************
white = -1
black = 1

left  = 1
right = 0

-- ****************************************************
-- Main
-- ****************************************************

oska_v4l7 :: [String] -> Char -> Int -> [String]
oska_v4l7 currentState player depth = (stateToString_v4l7 (minimaxSearch_v4l7(stringToState_v4l7 currentState) "MAX" depth))

-- ****************************************************
-- Process Output Signal -- Not perfectly done time crunch
-- got me (V4L7)
-- ****************************************************
stateToString_v4l7 :: State -> [String]
stateToString_v4l7 input = repB
  where
    repB = replaceBlacks_v4l7 (blacks input) repW
    repW = (replaceWhites_v4l7 (whites input) brd)
    brd  = getStringBoard_v4l7 (getBoardByRow_v4l7 (board input) (maxY_v4l7 (board input) 0))

replaceBlacks_v4l7 :: [ Point ] -> [String] -> [String]
replaceBlacks_v4l7 input brd
  | null (tail input)   = replacement
  | otherwise           = replaceBlacks_v4l7 (tail input) replacement
  where
    replacement = (replacePointWStr_v4l7 (x (head input)) (y (head input)) 'B' brd)

replaceWhites_v4l7 :: [ Point ] -> [String] -> [String]
replaceWhites_v4l7 input brd
  | null (tail input)   = replacement
  | otherwise           = replaceWhites_v4l7 (tail input) replacement
  where
    replacement = (replacePointWStr_v4l7 (x (head input)) (y (head input)) 'W' brd)

replacePointWStr_v4l7 :: Int -> Int -> Char -> [String] -> [String]
replacePointWStr_v4l7 xCo yCo rep list
  | yCo == 1            = (replaceInLine_v4l7 xCo rep (head list)) : (tail list)
  | otherwise           = (head list) : (replacePointWStr_v4l7 xCo (yCo-1) rep (tail list) )


replaceInLine_v4l7 :: Int -> Char -> String ->String
replaceInLine_v4l7 xCo rep list
  | null (tail list)      = rep : (tail list)
  | xCo == 2              = rep : (tail list)
  | otherwise             = (head list) : (replaceInLine_v4l7 (xCo - 1) rep (tail list))

maxY_v4l7 :: [Point] -> Int -> Int
maxY_v4l7 input bigest
  | null input	    	        = bigest
  | (y (head input)) > bigest	= maxY_v4l7 (tail input) (y (head input))
  | otherwise             		= maxY_v4l7 (tail input) bigest

getStringBoard_v4l7 :: [ [ Point ] ] -> [ String ]
getStringBoard_v4l7 list
  | null list         = []
  | otherwise         = (rowToString_v4l7 (head list)) : (getStringBoard_v4l7 (tail list))

rowToString_v4l7 :: [ Point ] -> String
rowToString_v4l7 input
  | null input        = []
  | otherwise         = '-' : rowToString_v4l7 (tail input)

getBoardByRow_v4l7 :: [Point] -> Int -> [ [Point] ]
getBoardByRow_v4l7 input numRows
  | numRows == 0       = []
  | otherwise          = first : next
    where
      first = (getRowFromBoard_v4l7 (input) numRows [])
      next = getBoardByRow_v4l7 input (numRows -1)

getRowFromBoard_v4l7 :: [ Point ] -> Int -> [Point] -> [ Point ]
getRowFromBoard_v4l7 input rowNum retList
  | null input        			= retList
  | (y (head input)) == rowNum 	= getRowFromBoard_v4l7 (tail input) rowNum ((head input) : retList)
  | otherwise         			= getRowFromBoard_v4l7 (tail input) rowNum (retList)




-- ****************************************************
-- Search Level Logic
-- ****************************************************

--1. Generate the game tree to as many levels (plies) that time and space
---- constraints allow. The top level is called MAX (as in it’s now MAX’s
---- turn to move), the next level is called MIN, the next level is MAX, and
---- so on.
--2. Apply the evaluation function to all the terminal (leaf) states/boards
---- to get “goodness” values.
--3. Use those terminal board values to determine the values to be
---- assigned to the immediate parents:
---- a) if the parent is at a MIN level, then the value is
---- the minimum of the values of its children
---- b) if the parent is at a MAX level, then the value is
---- the maximum of the values of its children
--4. Keep propagating values upward as in step 3
-- apply minOrMax to every level of the tree
--5. When the values reach the top of the game tree, MAX chooses the
---- move indicated by the highest value

minimaxSearch_v4l7 :: State -> String -> Int -> State
minimaxSearch_v4l7 state level depth
  | (depth == 1)				= (maxState_v4l7 (applyToLeaf_v4l7 (movesForState_v4l7 state)))
  | otherwise					= (maxState_v4l7
									(minOrMax_v4l7
										(propogateMinOrMax_v4l7
										(generateTree_v4l7 (movesForState_v4l7 state) "MIN" (depth - 1)) level depth)
									level))

generateTree_v4l7 :: [State] -> String -> Int -> [ [ [ State ] ] ]
generateTree_v4l7 states level depth
  | null states						= []
  | (depth == 0)					= []
  | otherwise						= thisLevel : nextLevel
    where
	  thisLevel = (generateLevel_v4l7 (movesForState_v4l7 (head states)))
	  nextLevel = (generateTree_v4l7 (tail states) (switchLevel_v4l7 level) (depth - 1))

switchLevel_v4l7 :: String -> String
switchLevel_v4l7 level
  | (level == "MAX")				= "MIN"
  | (level == "MIN")				= "MAX"

generateLevel_v4l7 :: [ State ] -> [ [ State ] ]
generateLevel_v4l7 states
  | null states             = []
  | otherwise               = new : next
  where
    new  = (movesForState_v4l7 (head states))
    next = (generateLevel_v4l7 (tail states))


propogateMinOrMax_v4l7 :: [ [ [State] ] ] -> String -> Int -> [ [State] ]
propogateMinOrMax_v4l7 treeOfStates level depth
  | (depth == 1)			= (head treeOfStates)
  | otherwise				= (minOrMax_v4l7 (applyToLeaves_v4l7 (head treeOfStates)) level)
								: (propogateMinOrMax_v4l7 (tail treeOfStates) (switchLevel_v4l7 level) (depth - 1))

applyToLeaves_v4l7 :: [ [State] ] -> [ [State] ]
applyToLeaves_v4l7 bottomLevel = (map applyToLeaf_v4l7 bottomLevel)

applyToLeaf_v4l7 :: [State] -> [State]
applyToLeaf_v4l7 bottomNode = (map boardEvaluator_v4l7 bottomNode)

minOrMax_v4l7 :: [ [State] ] -> String -> [State]
minOrMax_v4l7 states level
  | (level == "MAX")				= (map maxState_v4l7 states)
  | (level == "MIN")				= (map minState_v4l7 states)


maxState_v4l7 :: [State] -> State
maxState_v4l7 states
  | ((head states) == (last states))											= (head states)
  | (length states == 2)														= (gState_v4l7 (head states) (head (tail states)))
  | ((value (head states)) > (value (head (tail states))))						= (maxState_v4l7 ((head states) : (tail (tail states))))
  | otherwise																	= (maxState_v4l7 (tail states))

gState_v4l7 :: State -> State -> State
gState_v4l7 state1 state2
  | ((value state1) > (value state2))			= state1
  | otherwise									= state2


minState_v4l7 :: [State] -> State
minState_v4l7 states
  | ((head states) == (last states))											= (head states)
  | (length states == 2)														= (lState_v4l7 (head states) (head (tail states)))
  | ((value (head states)) < (value (head (tail states))))						= (minState_v4l7 ((head states) : (tail (tail states))))
  | otherwise																	= (minState_v4l7 (tail states))

lState_v4l7 :: State -> State -> State
lState_v4l7 state1 state2
  | ((value state1) < (value state2))			= state1
  | ((value state1) > (value state2))			= state2

-- instead of turning a state into an integer, now updates state with calculated value
boardEvaluator_v4l7 :: State -> State
boardEvaluator_v4l7 state
  | ((turn state) == white)		= whiteBoardEvaluator_v4l7 state
  | ((turn state) == black)		= blackBoardEvaluator_v4l7 state

whiteBoardEvaluator_v4l7 :: State -> State
whiteBoardEvaluator_v4l7 state = State (whites state) (blacks state) (board state) (turn state) newValue
	where newValue = ((piecesDiff_v4l7 (countWhites_v4l7 state (whites state)) (countBlacks_v4l7 state (blacks state)))
				+ (whiteJumpDiff_v4l7 state) + (whiteMoves_v4l7 state) + (whiteEndMoveDiff_v4l7 state))
				* (whiteWinningValue_v4l7 state) * (whiteLosingValue_v4l7 state)

piecesDiff_v4l7 :: Int -> Int -> Int
piecesDiff_v4l7 x y = x - y

whiteJumpDiff_v4l7 :: State -> Int
whiteJumpDiff_v4l7 state = (countJumps_v4l7 (whites state) state) - (countJumps_v4l7 (blacks state) state)

countJumps_v4l7 :: [Point] -> State -> Int
countJumps_v4l7 pieces state
  | null pieces				= 0
  | otherwise				= (heurJump_v4l7 (head pieces) state) + (countJumps_v4l7 (tail pieces) state)

heurJump_v4l7 :: Point -> State -> Int
heurJump_v4l7 piece state
  | twoJumps				= 2
  | oneJump					= 1
  | otherwise				= 0
	where
	  twoJumps = ((canJump_v4l7 piece right state) && (canJump_v4l7 piece left state))
	  oneJump = ((canJump_v4l7 piece right state) || (canJump_v4l7 piece left state))

whiteMoves_v4l7 :: State -> Int
whiteMoves_v4l7 state = (length (movesForState_v4l7 state))

whiteEndMoveDiff_v4l7 :: State -> Int
whiteEndMoveDiff_v4l7 state = ((backRowWhites_v4l7 (whites state) state) - (backRowBlacks_v4l7 (blacks state) state))

backRowWhites_v4l7 :: [Point] -> State -> Int
backRowWhites_v4l7 pieces state
  | null pieces										= 0
  | ((y (head pieces)) == (y (last (board state))))	= (backRowWhites_v4l7 (tail pieces) state) + 1
  | otherwise										= 0

backRowBlacks_v4l7 :: [Point] -> State -> Int
backRowBlacks_v4l7 pieces state
  | null pieces										= 0
  | ((y (head pieces)) == (y (last (board state))))	= (backRowBlacks_v4l7 (tail pieces) state) + 1
  | otherwise										= 0

whiteWinningValue_v4l7 :: State -> Int
whiteWinningValue_v4l7 state
  | ((hasWhiteWon_v4l7 state) == True)	= 10
  | otherwise							= 1

hasWhiteWon_v4l7 :: State -> Bool
hasWhiteWon_v4l7 state
  | allBlackGone			= True
  | allBackRow				= True
  | otherwise				= False
	where
	  allBlackGone = ((countWhites_v4l7 state (whites state)) > (countBlacks_v4l7 state (blacks state)))
						&& ((countBlacks_v4l7 state (blacks state)) == 0)
	  allBackRow = (countWhites_v4l7 state (whites state)) == (backRowWhites_v4l7 (whites state) state)

whiteLosingValue_v4l7 :: State ->Int
whiteLosingValue_v4l7 state
  | ((hasWhiteLost_v4l7 state) == True)	= -10
  | otherwise						= 1

hasWhiteLost_v4l7 :: State -> Bool
hasWhiteLost_v4l7 state
  | ((hasBlackWon_v4l7 state) == True)		= True
  | otherwise							= False

countWhites_v4l7 :: State -> [Point] -> Int
countWhites_v4l7 state whitePieces
  | null whitePieces		 	= 0
  | otherwise					= 1 + (countWhites_v4l7 state (tail whitePieces))

blackBoardEvaluator_v4l7 :: State -> State
blackBoardEvaluator_v4l7 state  = State (whites state) (blacks state) (board state) (turn state) newValue
	where newValue = ((piecesDiff_v4l7 (countBlacks_v4l7 state (blacks state)) (countWhites_v4l7 state (whites state)))
					+ (blackJumpDiff_v4l7 state) + (blackMoves_v4l7 state) + (blackEndMoveDiff_v4l7 state))
					* (blackWinningValue_v4l7 state) * (blackLosingValue_v4l7 state)

countBlacks_v4l7 :: State -> [Point] -> Int
countBlacks_v4l7 state blackPieces
  | null blackPieces		 	= 0
  | otherwise					= 1 + (countBlacks_v4l7 state (tail blackPieces))

blackJumpDiff_v4l7 :: State -> Int
blackJumpDiff_v4l7 state = (countJumps_v4l7 (blacks state) state) - (countJumps_v4l7 (whites state) state)

blackMoves_v4l7 :: State -> Int
blackMoves_v4l7 state = (length (movesForState_v4l7 state))

blackEndMoveDiff_v4l7 :: State -> Int
blackEndMoveDiff_v4l7 state = ((backRowBlacks_v4l7 (blacks state) state) - (backRowWhites_v4l7 (whites state) state))

blackWinningValue_v4l7 :: State ->Int
blackWinningValue_v4l7 state
  | ((hasBlackWon_v4l7 state) == True)	= 10
  | otherwise						= 1

hasBlackWon_v4l7 :: State -> Bool
hasBlackWon_v4l7 state
  | allWhiteGone			= True
  | allBackRow				= True
  | otherwise				= False
	where
	  allWhiteGone = ((countBlacks_v4l7 state (blacks state)) > (countWhites_v4l7 state (whites state)))
					&& ((countWhites_v4l7 state (whites state)) == 0)
	  allBackRow = (countBlacks_v4l7 state (blacks state)) == (backRowBlacks_v4l7 (blacks state) state)

blackLosingValue_v4l7 :: State ->Int
blackLosingValue_v4l7 state
  | ((hasBlackLost_v4l7 state) == True)	= -10
  | otherwise						= 1

hasBlackLost_v4l7 :: State -> Bool
hasBlackLost_v4l7 state
  | ((hasWhiteWon_v4l7 state) == True)		= True
  | otherwise								= False


---- ****************************************************
---- State Level Logic
---- ****************************************************
-- V4L7
movesForState_v4l7 :: State -> [ State ]
movesForState_v4l7 state
  | (turn state) == white   = possibleMoves_v4l7 (whites state) state
  | otherwise               = possibleMoves_v4l7 (blacks state) state
-- V4L7
possibleMoves_v4l7 :: [ Point ] -> State -> [ State ]
possibleMoves_v4l7 pts state
  | null pts                = []
  | otherwise               = merge_v4l7 validMoves next
  where
    validMoves = (validMovesForPoint_v4l7 (head pts) state)
    next       = (possibleMoves_v4l7 (tail pts) state)

-- ****************************************************
-- Point Interactions
-- ****************************************************
-- V4L7
validMovesForPoint_v4l7 :: Point -> State -> [ State ]
validMovesForPoint_v4l7 pt state = merge_v4l7 jumpStates moveStates
  where
    jumpStates = jumpStatesForPoint_v4l7 pt state
    moveStates = moveStateForPoint_v4l7 pt state
-- V4L7
moveStateForPoint_v4l7 :: Point -> State -> [ State ]
moveStateForPoint_v4l7 pt state
  | both              = [leftMove, rightMove]
  | leftMove /= state = [leftMove]
  | rightMove/= state = [rightMove]
  | otherwise         = [State [(Point 999 999)] [(Point 999 999)] [(Point 999 999)] white (value state)]
  where
    both      = (leftMove /= state) && (rightMove /= state)
    leftMove  = stateAfterMove_v4l7 pt left  state
    rightMove = stateAfterMove_v4l7 pt right state
-- V4L7
jumpStatesForPoint_v4l7 :: Point -> State -> [ State ]
jumpStatesForPoint_v4l7 pt state
  | both              = [leftJump, rightJump]
  | leftJump /= state = [leftJump]
  | rightJump/= state = [rightJump]
  | otherwise         = []
  where
    both      = (leftJump /= state) && (rightJump /= state)
    leftJump  = stateAfterJump_v4l7 pt left  state
    rightJump = stateAfterJump_v4l7 pt right state
-- V4L7
stateAfterJump_v4l7 :: Point -> Int -> State -> State
stateAfterJump_v4l7 pt dir state
  | whiteJump         = State (whites next) (blacks cleanState) (board state) black (value state)
  | blackJump         = State (whites cleanState) (blacks next) (board state) white (value state)
  | otherwise         = state
  where
    whiteJump   = (canJump_v4l7 pt dir state) && ( (turn state) == white )
    blackJump   = (canJump_v4l7 pt dir state) && ( (turn state) == black )
    next        = replacePointForState_v4l7 pt ( jump_v4l7 pt dir ( turn state ) ) state
    cleanState  = removePointForState_v4l7 ( move_v4l7 pt dir ( turn state ) ) state
-- V4L7
stateAfterMove_v4l7 :: Point -> Int -> State -> State
stateAfterMove_v4l7 pt dir state
  | (canMove_v4l7 pt dir state)  = replacePointForState_v4l7 pt ( move_v4l7 pt dir ( turn state ) ) state
  | otherwise               = state
-- V4L7
replacePointForState_v4l7 :: Point -> Point -> State -> State
replacePointForState_v4l7 pt newPt state
  | elem pt ( whites state ) = State (replacePoint_v4l7 pt newPt (whites state)) (blacks state) (board state) nextTurn (value state)
  | elem pt ( blacks state ) = State (whites state) (replacePoint_v4l7 pt newPt (blacks state)) (board state) nextTurn (value state)
  | otherwise                = state
  where
    nextTurn = ( (-1) * (turn state) )
-- V4L7
removePointForState_v4l7 :: Point -> State -> State
removePointForState_v4l7 pt state
  | elem pt ( whites state ) = State (removePoint_v4l7 pt (whites state)) (blacks state) (board state) (turn state) (value state)
  | elem pt ( blacks state ) = State (whites state) (removePoint_v4l7 pt (blacks state)) (board state) (turn state) (value state)
  | otherwise                = state

-- V4L7
removePoint_v4l7 :: Point ->[ Point ] -> [ Point ]
removePoint_v4l7 pt pts
  | (null pts)                    = []
  | pt == (head pts)              = (tail pts)
  | otherwise                     = (head pts) : removePoint_v4l7 pt (tail pts)
-- V4L7
replacePoint_v4l7 :: Point -> Point -> [ Point ] -> [ Point ]
replacePoint_v4l7 pt newPt pts
  | (null pts)                    = []
  | pt == (head pts)              = newPt : (tail pts)
  | otherwise                     = (head pts) : (replacePoint_v4l7 pt newPt (tail pts))

-- ****************************************************
-- Point Level Logic (Eg. Moves)
-- ****************************************************
-- V4L7
canJump_v4l7 :: Point -> Int -> State -> Bool
canJump_v4l7 pt dir state = bounded && blocked && clear
  where
    nextPoint = jump_v4l7 pt dir (turn state)
    blocked   = elem (move_v4l7 pt dir (turn state)) (otherPts_v4l7 state)
    bounded   = (pointInBounds_v4l7 nextPoint state)
    clear     = not (pointTaken_v4l7 nextPoint state)
-- V4L7
canMove_v4l7 :: Point -> Int -> State -> Bool
canMove_v4l7 pt dir state = bounded && clear
  where
    nextPoint = move_v4l7 pt dir (turn state)
    bounded   = (pointInBounds_v4l7 nextPoint state)
    clear     = not (pointTaken_v4l7 nextPoint state)
-- V4L7
pointInBounds_v4l7 :: Point -> State -> Bool
pointInBounds_v4l7 point state = ( elem point (board state) )

-- V4L7
pointTaken_v4l7 ::  Point -> State -> Bool
pointTaken_v4l7 point state  = ( elem point (blacks state) ) || ( elem point (whites state) )

-- currentPoint, direction, turn, return movedPoint
move_v4l7 :: Point -> Int -> Int -> Point
move_v4l7 old direction turn
  | turn == white     = ( Point ( (x old) + direction ) ( (y old) + 1 ) )
  | otherwise         = ( Point ( (x old) + direction ) ( (y old) - 1 ) )

-- currentPoint, direction, turn, return movedPoint
jump_v4l7 :: Point -> Int -> Int -> Point
jump_v4l7 old direction turn
  | turn == white     = ( Point ( (x old) + 2 * direction ) ( (y old) + 2 ) )
  | otherwise         = ( Point ( (x old) - 2 * direction ) ( (y old) - 2 ) )

-- ****************************************************
-- Process Input Signal - Assumes only initial states
-- ****************************************************
-- V4L7
stringToState_v4l7 :: [String] -> State
stringToState_v4l7 input = ( State whites blacks board white 0 )
  where
    whites = (buildWhites_v4l7 (head input) )
    blacks = (buildBlacks_v4l7 (last input) (length input) )
    board  = (buildBoard_v4l7 input ( length (head input) ) (length input) )
-- V4L7
buildWhites_v4l7 :: String -> [ Point ]
buildWhites_v4l7 input
  | null input        = []
  | otherwise         = (Point (length input) 1) : buildWhites_v4l7 (tail input)
-- V4L7
buildBlacks_v4l7 :: String -> Int -> [ Point ]
buildBlacks_v4l7 input height
  | null input        = []
  | otherwise         = (Point (length input) height) : (buildBlacks_v4l7 (tail input) height)
-- V4L7
buildBoard_v4l7 :: [ String ] -> Int -> Int -> [ Point ]
buildBoard_v4l7 input maxWidth height
  | null input        = []
  | otherwise         = merge_v4l7 ( processLine_v4l7 (head input) (length input) offset ) next
  where
    next      = ( buildBoard_v4l7 (tail input) maxWidth height )
    offset    = ( maxWidth - (length (head input) ) )
-- V4L7
processLine_v4l7 :: String -> Int -> Int -> [ Point ]
processLine_v4l7 line yCoord offset
  | null line           = []
  | otherwise           = pt : next
  where
    pt    = ( Point xCoord yCoord )
    next  = ( processLine_v4l7 (tail line) yCoord offset )
    xCoord= ( offset + (length line) )

-- ****************************************************
-- Helpers
-- ****************************************************
-- V4L7
merge_v4l7 :: [a] -> [a] -> [a]
merge_v4l7 [] []               = []
merge_v4l7 xs []               = xs
merge_v4l7 [] ys               = ys
merge_v4l7 (x:xs) (y:ys)       = x : y : merge_v4l7 xs ys

-- V4L7
nth_v4l7 :: [a] -> Int -> Int -> a
nth_v4l7 arr i depth
  | i == depth  = (head arr)
  | otherwise   = nth_v4l7 (tail arr) (i+1) depth

-- V4L7
otherPts_v4l7 state
  | (turn state) == white = (blacks state)
  | otherwise             = (whites state)

-- ****************************************************
-- Tests - using my own system that I've made up on the
-- fly here... probs not the best solution but it works
-- ****************************************************

-- All tests written by V4L7
testSuite_v4l7       = functionalTests_v4l7 && unitTests_v4l7
functionalTests_v4l7 = t_validMovesForPoint_v4l7 && t_removePointForState_v4l7 && t_removePointForState_v4l7
unitTests_v4l7       = t_canJump_v4l7 && t_canMove_v4l7 && t_pointTaken_v4l7 && t_pointInBounds_v4l7 && t_move_v4l7 && t_jump_v4l7

-- validMovesForPoint :: Point -> State -> [ State ]
t_validMovesForPoint_v4l7 = trueTests && (not falseTests)
  where
    trueTests = t_validMovesForPoint1_v4l7 && t_validMovesForPoint2_v4l7
    falseTests= False

t_validMovesForPoint1_v4l7 = correctLength && newStates && newTurns
  where
    validMoves    = validMovesForPoint_v4l7 (head whiteList) firstState
    correctLength = (length validMoves) == 1
    newStates      = not (elem firstState validMoves)
    newTurns      = not (elem (turn firstState) (map turn validMoves))

t_validMovesForPoint2_v4l7 = correctLength && newStates && newTurns
  where
    validMoves    = (validMovesForPoint_v4l7 (nth_v4l7 whiteList 0 1) firstState)
    correctLength = (length validMoves) == 2
    newStates      = not (elem firstState validMoves)
    newTurns      = not (elem (turn firstState) (map turn validMoves))


-- stateAfterMove :: Point -> Int -> State (returns inital if not available)
t_stateAfterMoveWL_v4l7 = same && newTurn
  where
    same        = movedState == blockedStateW
    movedState  = (stateAfterMove_v4l7 (head (whites blockedStateW)) left  blockedStateW)
    newTurn     = (turn blockedStateW) /= (turn movedState)

t_stateAfterMoveBL_v4l7 = stateAfterMove_v4l7 (head (blacks blockedStateB)) left  blockedStateB
t_stateAfterMoveWR_v4l7 = stateAfterMove_v4l7 (head (whites firstState)) right firstState
t_stateAfterMoveBR_v4l7 = stateAfterMove_v4l7 (head (blacks firstState)) right firstState

-- stateAfterJump :: Point -> Int -> State (returns inital if not available)
t_stateAfterJumpWL_v4l7 = stateAfterJump_v4l7 (head (whites blockedStateW)) left  blockedStateW
t_stateAfterJumpBL_v4l7 = stateAfterJump_v4l7 (head (blacks blockedStateB)) left  blockedStateB
t_stateAfterJumpWR_v4l7 = stateAfterJump_v4l7 (last (whites blockedStateW)) right blockedStateW

-- replacePointForState :: Point -> Point -> State -> State
t_replacePointForState_v4l7 = elem (Point 10 10) newWhite
  where newWhite = (whites (replacePointForState_v4l7 (head whiteList) (Point 10 10) firstState))

-- removePointForState :: Point -> State -> State
t_removePointForState_v4l7 = not (elem (head whiteList) (whites (removePointForState_v4l7 (head whiteList) firstState)))

-- removePoint :: Point ->[ Point ] -> [ Point ]
t_removePointH_v4l7   = removePoint_v4l7 (head whiteList) whiteList
t_removePointL_v4l7   = removePoint_v4l7 (last whiteList) whiteList

-- replacePoint :: Point -> Point -> [ Point ] -> [ Point ]
t_replacePointH_v4l7  = replacePoint_v4l7 (head whiteList) (Point 10 10) whiteList
t_replacePointL_v4l7  = replacePoint_v4l7 (last whiteList) (Point 10 10) whiteList

--
-- canMove :: Point -> Int -> State -> Bool
-- Test Cases:
-- Valid
-- Out of Bounds
-- Blocked
--
t_canMove_v4l7     = trueTests && (not falseTests)
  where
    trueTests = t_canMoveT_v4l7
    falseTests= t_canMoveFB_v4l7 && t_canMoveFOOB_v4l7

t_canMoveT_v4l7    = canMove_v4l7 (head (whites firstState)) right firstState
t_canMoveFOOB_v4l7 = canMove_v4l7 (head (whites firstState)) left firstState
t_canMoveFB_v4l7   = canMove_v4l7 (head (whites blockedStateW)) right blockedStateW

--
-- canJump :: Point -> Int -> State -> Bool
-- Test Cases:
-- white jump black (T)
-- black jump white (T)
-- white jump white (F)
-- Jump Destination Taken (F)
-- Out of Bounds (F)
-- Jump Not Blocked (F)
--
t_canJump_v4l7     = trueTests && (not falseTests)
  where
    trueTests = t_canJumpWJB_v4l7 && t_canJumpBJW_v4l7
    falseTests= (t_canJumpWJW_v4l7) && (t_canJumpJDT_v4l7) && (t_canJumpOOB_v4l7) && (t_canJumpNB_v4l7)

t_canJumpWJB_v4l7  = canJump_v4l7 (head (whites blockedStateW)) right blockedStateW
t_canJumpBJW_v4l7  = canJump_v4l7 (last (blacks blockedStateB)) right blockedStateB
t_canJumpWJW_v4l7  = canJump_v4l7 (last (whites blockedStateW)) right blockedStateW
t_canJumpJDT_v4l7  = canJump_v4l7 (last (whites blockedStateW)) right blockedStateW
t_canJumpOOB_v4l7  = canJump_v4l7 (head (whites blockedStateW)) left  blockedStateW
t_canJumpNB_v4l7   = canJump_v4l7 (head (whites firstState)) right firstState

--
-- pointTaken ::  Point -> State -> Bool
--
t_pointTaken_v4l7  = t_pointTakenT_v4l7 && (not t_pointTakenF_v4l7)

t_pointTakenT_v4l7 = pointTaken_v4l7 (Point 4 3) blockedStateW
t_pointTakenF_v4l7 = pointTaken_v4l7 (Point 1 1) blockedStateW

--
-- pointInBounds :: Point -> State -> Bool
--
t_pointInBounds_v4l7  = t_pointInBoundsT_v4l7 && (not t_pointInBoundsF_v4l7)

t_pointInBoundsT_v4l7 = pointInBounds_v4l7 (head whiteList) firstState
t_pointInBoundsF_v4l7 = pointInBounds_v4l7 (Point 99 99) firstState

--
-- move :: Point -> Int -> Int -> Point
--
t_move_v4l7 = trueTests
  where
    trueTests = t_moveLW_v4l7 && t_moveRW_v4l7 && t_moveLB_v4l7 && t_moveRB_v4l7

t_moveLW_v4l7 = (move_v4l7 p1 left  white) == (Point 2 2)
t_moveRW_v4l7 = (move_v4l7 p1 right white) == (Point 1 2)
t_moveLB_v4l7 = (move_v4l7 p5 left  black) == (Point 2 4)
t_moveRB_v4l7 = (move_v4l7 p5 right black) == (Point 1 4)

--
-- jump :: Point -> Int -> Int -> Point
--
t_jump_v4l7 = trueTests
  where
    trueTests = t_jumpLW_v4l7 && t_jumpRW_v4l7 && t_jumpLB_v4l7 && t_jumpRB_v4l7

t_jumpLW_v4l7 = (jump_v4l7 p1 left  white) == (Point 3 3)
t_jumpRW_v4l7 = (jump_v4l7 p1 right white) == (Point 1 3)
t_jumpLB_v4l7 = (jump_v4l7 p8 left  black) == (Point 2 3)
t_jumpRB_v4l7 = (jump_v4l7 p5 right black) == (Point 1 3)

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

firstState = stringToState_v4l7 testString

blockedStateW = ( State [ (Point 3 2) , (Point 3 1) ] [ (Point 4 3),(Point 3 3) ] testBoard white 0)
blockedStateB = ( State [ (Point 3 2) ] [ (Point 4 3),(Point 3 3) ] testBoard black 0)

testBoard = [(Point 3 5),(Point 4 5),(Point 2 5),(Point 1 5),(Point 4 4),(Point 3 4),(Point 2 4),(Point 4 3),(Point 3 3),(Point 4 2),(Point 3 2),(Point 2 2),(Point 4 1),(Point 3 1),(Point 2 1),(Point 1 1)]

testString = [ "wwww", "---", "--",  "---", "bbbb" ]

