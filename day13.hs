import Data.Bits
import qualified Data.HashPSQ as Q
import qualified Data.HashSet as S
import Debug.Trace

createLabyrinth :: Int -> [[Bool]]
createLabyrinth favourite = map row [0..]
  where row y = map (isWall favourite y) [0..]

isWall :: Int -> Int -> Int -> Bool
isWall favourite y x
  | y < 0 || x < 0 = True
  | otherwise = odd.popCount $ favourite + x*x + 3*x + 2*x*y + y + y*y

type Pos = (Int, Int)
type Dist = Int

astarDist :: Int -> Pos -> Dist
astarDist favorite goal = astar' start S.empty
  where 
    labyrinth = createLabyrinth favorite
    start = Q.insert (1, 1) 0 0 Q.empty
    astar' queue visited
      | goal == position = distance
      | otherwise = astar' queue'' visited'
      where 
        (Just (position, guess, distance, queue')) = Q.minView queue

        neighbors = getNeighbors position goal distance queue' labyrinth favorite
        neighbors' = filter (not . flip S.member visited . pos) neighbors
        pos (a, _, _) = a

        y' :: Int
        y' = maximum $ map (flip trace (snd position)) (drawOverlay favorite visited 40 50)
        pos' = (fst position, y')

        visited' = S.insert pos' visited
        queue'' = insertAll queue' neighbors'

getNeighbors (x, y) (gx, gy) dist q l favorite = [((x', y'), calcDist x' y', dist+1) |
                                                  dx <- [-1..1], dy <- [-1..1],
                                                  abs(dx+dy)==1,
                                                  let x' = x+dx,
                                                  let y' = y+dy,
                                                  not $ isWall favorite y' x']
  where calcDist x' y' = dist + abs (x' - gx) + abs (gy - y')

insertAll q n = foldl insert' q n
  where insert' q (pos, guess, dist) = Q.insert pos guess dist q

drawOverlay favourite set mx my = (++["\n"]) $
                                  do (y, line) <- zip [0..] base
                                     return $ do
                                       (x, c) <- zip [0..] line
                                       if S.member (x, y) set
                                       then return 'O'
                                       else return c
  where base = drawLabyrinth favourite mx my

showLabyrinth seed mx my = mapM_ putStrLn  $ drawLabyrinth seed mx my
drawLabyrinth :: Int -> Int -> Int -> [[Char]]
drawLabyrinth seed mx my = draw . take my . map (take mx) $ createLabyrinth seed 
  where draw = map . map $ \c -> if c then '#' else '.'

-- #OO###O#...#..#....##.#...#....#
-- #O#..#O##.#######.#.#.####.##.##
-- #OO#.#O##.#....##...#..#.####.##
-- ##OO##O##.#..#...###.#.....#....
-- OO#O##OO########.#.#####...#..##
-- #OOOOOOOOOOOO###.....#####.#####
-- ###O##O####OOO#.##....#..#.#..#.
-- #.#OO#O##O#O#O######..#.##..#.##
-- .###OOOOOO#OO#..#OO####.###..#..
-- .####OO###.#O##.#O#...#..#.#.##.
-- ..######O###OO###OO##.#..###..##
-- #..##..#OOOOOOO###O#..###......#
-- .......##O##O#OOOOO#....#.##.#.#
-- #####.#.#O##OOO###O##...##.#..#.
-- ...##..##O#.###..#O###.#.##.#..#
-- .#.###.#OO#...#.##OOO#..#.#..#..
-- ##..#..#O######.#####.#..###..##
-- ....##O#O##OO#...##.#.##.####..#
-- ##...#OOOOOOO#......#.....#.###.
-- ###.#######O########.##.#.###.##
-- .##.#...###O##OOO#.##.#..#.....#
-- .##.#.....#OOOO#OO#.##.#.####.#.
-- ...###.##.#O###.#O##.###...##..#
-- .#..##.#..##..##OOO#.....#..##.#
-- ###....#...###.#OOO##.#####.#..#
-- .#.##.###.#..#.##O#.###OO#..##.#
-- .####..##..#.#.##OO#OOOOO#...#.#
-- ...#.#...#..##..##OOO###O##.##..
-- #..##.##..#.###.#.####.#O##O###.
-- ##..#######..#..#.....##OOOOO#..
-- .###..#......#..###.#.#.###OO#..
-- #..##.#..########.###.###.##O##.
-- .#...####....#.......#...#.#O###
-- ...#....#..#.#.##..#.###..##O##.
-- ############.#.#####....#.#.OOOO
-- #..##..#....##..#....##.#.#OO##O
-- #....#.##.#.#.#.#.###.#..######O
-- ##.#..#.#.#.##..##..##.#.....#OO
-- .##.#...#.##.#...#.#.######..#O#
-- #.#..###...#.##.##.##..##.#..#..
