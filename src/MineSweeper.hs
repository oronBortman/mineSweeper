module MineSweeper where
import System.Random


data Action = Dig Int Int | Flag Int Int deriving (Read)

type Coordinate = (Int, Int)

type BoardCells = [[(Coordinate, Char)]]

data Board = Board { width :: Int, height :: Int, numOfMines :: Int, theBoard :: BoardCells}

initBoard :: Int -> Int -> BoardCells
initBoard 0 y = [initLine 0 y]
initBoard x y = initBoard (x-1) y ++ [initLine x y]

initLine :: Int -> Int -> [(Coordinate, Char)]
initLine x 0 = [((x,0), ' ')]
initLine x y = initLine x (y-1) ++ [((x,y), ' ')]

randomCoord :: Int -> Int -> StdGen -> (Coordinate, StdGen)
randomCoord width height gen =
  let (randX, gen') = randomR (0, width - 1) gen
      (randY, gen'') = randomR (0, height - 1) gen'
  in ((randX, randY), gen'')

randomCoords :: Int -> Int -> Int -> StdGen -> [(Coordinate, StdGen)]
randomCoords _ _ 0 gen = []
randomCoords x y z gen =
  let ((a, b), gen') = randomCoord x y gen
  in [((a, b), gen')] ++ randomCoords x y (z-1) gen'


takeFromEnd :: Int -> [a] -> [a]
takeFromEnd x ys = reverse (take x (reverse ys))

modifyBoard :: Int -> Int -> Int -> Int -> Char -> BoardCells -> BoardCells
modifyBoard x y w h c board = (take x board) ++ [(take (y) (board !! x)) ++ [((x,y),c)] ++ (takeFromEnd (w-y) (board !! x))] ++ (takeFromEnd (h-x) board) 
