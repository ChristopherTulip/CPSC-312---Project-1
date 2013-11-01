import Data.List
--
-- Data Model
--

data State = State {
	white 	:: [ Piece ] 
	black 	:: [ Piece ]
	turn 	:: Int
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



--
-- Piece Level Logic (Eg. Moves)
--

canJump :: Piece -> State -> Bool
-- currentPiece, currentState, return canJump?

canMove :: Piece -> State -> Bool
-- currentPiece, currentState, return canMove?

move :: Piece -> Piece
-- currentPiece, return movedPiece

jump :: Piece -> Piece 
-- 

-- Constants
white = 0
black =1