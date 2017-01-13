module Day24 where
import Data.Graph.AStar
import qualified Data.HashSet as S
import qualified Data.Map as M
import Data.Char
import Control.Monad
import Data.List

type Pos = (Int, Int)
type Board = [String]

main = do
  board <- lines <$> readFile "in24.txt"
  let numbers = findNumbers board
  let paths = findDistances board numbers
  print $ findSolution (makePaths1 numbers) paths
  print $ findSolution (makePaths2 numbers) paths

findSolution paths distances = minimum $ costFunction <$> paths
  where costFunction ls = fmap sum . traverse distance $ pairwise ls
        distance (a, b) = distances M.! ordered a b
        pairwise ls = zip ls (tail ls)

makePaths1 = permutations . map fst
makePaths2 =  map (("0"++) . (++"0")) . permutations . (\\ "0") . map fst
        
findNumbers :: Board -> [(Char, Pos)]
findNumbers board = [ (c, (x, y)) 
                    | (line, y) <- zip board [0..]
                    , (c, x) <- zip line [0..]
                    , isDigit c]

findDistances :: Board -> [(Char, Pos)] -> M.Map (Char, Char) (Maybe Int)
findDistances board ls = M.fromList $ go ls
  where
    go [] = []
    go (x:xs) = findPairs x xs ++ go xs
    findPairs _ [] = []
    findPairs (c1, p1) ((c2, p2):ys) = (ordered c1 c2, length <$> findPath board p1 p2) : findPairs (c1, p1) ys  

ordered a b
 | a <= b = (a, b)
 | otherwise = (b, a)

findPath board from to = aStar (neighbors board) distance (estimate to) (isGoal to) from

neighbors :: Board -> Pos -> S.HashSet Pos
neighbors b (x, y) = S.fromList $ filter valid options
  where
   options = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
   onBoard (x, y) = x >= 0 && x < (length . head $ b) && y >= 0 && y < length b
   notWall (x, y) = b !! y !! x /= '#'
   valid = (&&) <$> onBoard <*> notWall 
distance = const . const $ 1
estimate (gx, gy) (x, y) = abs (gx - x) + abs (gy - y)
isGoal = (==)
