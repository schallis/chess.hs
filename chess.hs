import Data.Ix
import System.IO

{-

Basic Game of Chess
Steve Challis

Run like so:
$ ghci chess.hs
*Main> valid_moves testKing 
[(0,1),(1,0),(1,1)]

DATE        TIME    PROGRESS
18/11/10    00:30   Basic relative moves worked out
19/11/10    03:00   Culling impossible moves and generation of list of all moves
                    of all pieces from all positions; get possible moves for
                    particular piece and position

-}

type Position = (Int,Int)
type Move = Position
type PieceType = String
type PieceState = (PieceType, Position)
type PieceMoves = (PieceType, [Move])
type StateMoves = (Position, [Move])
type PieceStateMoves = (PieceType, [StateMoves])

-- Calculate possible moves for each piece, regardless of position
valid_relative_moves :: [PieceMoves]
valid_relative_moves
    = [ ("knight",  concat[[(x,y),(y,x)] | x <- [-1,1], y <- [-2,2]])
      , ("pawn",    [(0,1)])
      , ("king",    [(x,y) | x <- [-1,0,1], y <- [-1,0,1], (x,y) /= (0,0)])
      , ("queen",   [(x,y) | x <- [-7..7], y <- [-7..7], (x,y) /= (0,0),
                     abs x == abs y || x == 0 || y == 0])
      , ("rook",    [(x,y) | x <- [-7..7], y <- [-7..7],
                     (x,y) /= (0,0), x == 0 || y == 0])
      , ("bishop",  [(x,y) | x <- [-7..7], y <- [-7..7],
                     (x,y) /= (0,0), x == y])
      ]


-- Remove impossible moves from relative moves
cull_moves :: (PieceType, [Move]) -> PieceStateMoves
cull_moves (piece, moves)
    = ( piece,
        [ ((x,y),
           [ (mx,my) | (mx,my) <- moves
           , inRange (1,8) (x + mx)
           , inRange (1,8) (y + my)
           ]
          )
        | x <- [1..8]
        , y <- [1..8]
        ]
      )

-- Calculate all possible moves for each piece and position 
valid_absolute_moves :: [PieceStateMoves]
valid_absolute_moves = map cull_moves valid_relative_moves


-- Get possible moves from anywhere on the board for a given piece        
get_piece_moves :: PieceType -> [PieceStateMoves] -> [(Position, [Move])]
get_piece_moves piece ((p,x):vs) | vs == [] = []
                                 | piece == p = x
                                 | otherwise = get_piece_moves piece vs


-- Given a list of all possible moves for a piece and a position, get the
-- remaining possible moves
get_state_moves :: Position -> [(Position, [Move])] -> [Move]
get_state_moves pos ((ppos, moves):vs) | vs == [] = []
                                       | pos == ppos = moves
                                       | otherwise = get_state_moves pos vs


-- Get all possible moves the player can make for given piece state
valid_moves :: PieceState -> [Move]
valid_moves (p,pos) = get_state_moves pos piece_moves
                      where
                          piece_moves = get_piece_moves p valid_absolute_moves


-- Convert moves to positions
move_to_pos :: Move -> Position -> Position
move_to_pos (mx,my) (px, py) = (px+mx, py+my)

-- Graphically display board with list of pieces on it
-- and mark moves for one piece
--plot_board :: [PieceState] -> [Move] -> IO()
--plot_board pss ms
--    = printLn "--------"
--      printLn "--------"

testKing :: PieceState
testKing = ("king", (1,1))

