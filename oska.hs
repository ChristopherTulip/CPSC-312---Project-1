import Data.List

--
-- Data Model
--

data State = State {
	white 	:: [ Piece ] 
	black 	:: [ Piece ]
	turn 		:: Int
} deriving (Show, Eq, Ord)

data Piece = Piece {
	x 		:: Int
	y 		:: Int
} deriving (Show, Eq, Ord)

--
-- Search Level Logic
--

minimaxSearch :: State -> Int -> State
-- currentState, depth, return newState

nextStates :: State -> [ State ]
-- currentState, return nextStates


bestSate :: [ State ] -> State
-- nextStates, return bestState

--
-- State Level Logic
--

getHeuristicValue :: State -> Int
-- currentState, return value

movesForState :: State -> [ State ]
-- currentState, return possibleStates
--
-- for each piece get nextStates
--


-- 
-- Piece Interactions
--

movesForPiece :: Piece -> State -> [ State ]
-- pieceToMove, currentState, newStates
--
-- Possible Moves: 
-- canJump (left | right) -> (jump pieceToMove) / (removePiece jumped) : newStates
-- canMove (left | right) -> (move pieceToMove) : newStates
--


canJump :: Piece -> State -> Bool
-- currentPiece, currentState, return canJump?

canMove :: Piece -> State -> Bool
-- currentPiece, currentState, return canMove?

removePiece :: Piece -> State -> State
-- pieceToRemove, currentState, return newState

replacePiece :: Piece -> Piece -> State -> State
-- pieceToReplace, replacement, currentState, return newState 

--
-- Piece Level Logic (Eg. Moves)
--

move :: Piece -> Int -> Piece
-- currentPiece, direction, return movedPiece

jump :: Piece -> Int -> Piece 
-- currentPiece, direction, return movedPiece

--
-- Constants
--
white = 0
black = 1

left 	= 0
right = 1