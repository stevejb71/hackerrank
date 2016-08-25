module NPuzzle where

import Heap
import Search
import qualified Data.Vector as V
import Data.Vector ((!))
import Debug.Trace
import Prelude hiding (Left, Right)
import Data.Maybe (fromJust)

type Size = Int
type Piece = Int
type Pos = Int
type Score = Int
newtype Board = Board {runBoard :: V.Vector Piece} deriving (Eq, Show)
data Direction = Left | Right | Up | Down deriving (Eq, Show)

instance Ord Board where
  c1 <= c2 = priority c1 <= priority c2

instance HasPriority Board where
  priority b = V.sum . V.map (uncurry $ manhattanDistance n) $ posPieces
      where n = boardSize b
            indices = V.generate (V.length . runBoard $ b) id
            posPieces = V.zip indices (runBoard b)

boardSize :: Board -> Int
boardSize = round . sqrt . fromIntegral . V.length . runBoard

manhattanDistance :: Size -> Pos -> Piece -> Int
manhattanDistance n pos 0 = manhattanDistance n pos (n * n)
manhattanDistance n pos x = abs (rx - rp) + abs (cx - cp)
  where (rx, cx) = (x - 1) `divMod` n
        (rp, cp) = pos `divMod` n

move :: Board -> Pos -> Board
move (Board b) pos = Board $ V.update b updates
  where zeroIndex = fromJust . V.findIndex (== 0) $ b
        p = b ! pos
        updates = V.fromList [(zeroIndex, p), (pos, 0)]

nextMoves :: Board -> V.Vector (Pos, Direction)
nextMoves bd@(Board b) = V.fromList . concat $ [up, down, left, right]
  where zeroIndex = fromJust . V.findIndex (== 0) $ b
        n = boardSize bd
        (r, c) = zeroIndex `divMod` n
        left = if c > 0 then [(r * n + (c - 1), Left)] else []
        right = if c < n - 1 then [(r * n + (c + 1), Right)] else []
        up = if r > 0 then [((r - 1) * n + c, Up)] else []
        down = if r < n - 1 then [((r + 1) * n + c, Down)] else []

nextBoards :: Board -> V.Vector Board
nextBoards b = fmap (\(pos, _) -> move b pos) (nextMoves b)

solve :: Board -> Maybe Board
solve b = astarSearch b (\b -> priority b == 0) nextBoards
