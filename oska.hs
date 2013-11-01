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
-- State Level Logic
--

minimaxSearch :: State -> Int -> State
-- currentState, depth, return newState

nextStates :: State -> [ State ]
-- currentState, return nextStates

bestSate :: [ State ] -> State
-- nextStates, return bestState

possibleMoves :: Piece -> State -> [ State ]
-- pieceToMove, currentState, newStates

-- 
-- Piece Interactions
--


canJump :: Piece -> State -> Bool
-- currentPiece, currentState, return canJump?

canMove :: Piece -> State -> Bool
-- currentPiece, currentState, return canMove?

removePiece :: Piece -> State -> State
-- pieceToRemove, currentState, return newState

replacePiece :: Piece -> State -> State
-- pieceToReplace, replacement, currentState, return newState 

--
-- Piece Level Logic (Eg. Moves)
--

move :: Piece -> Int -> Piece
-- currentPiece, direction, return movedPiece

jump :: Piece -> Piece 
-- This function 

jumpRight :: Piece -> Piece 
-- This function 


-- Constants
white = 0
black = 1

left 	= 0
right = 1