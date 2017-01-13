import Data.Hash.MD5
import Data.List (findIndices, maximumBy, minimumBy)
import Data.Ord

longestPath  = pathOn length
shortestPath = pathOn (Down . length)
pathOn v = ((,) <*> length) . maximumBy (comparing v) . allPaths

allPaths seed = go "" (0, 0)
  where
    go path pos = do
      (pos', dir) <- getNeighbors pos . findOpen $ seed++path
      let path' = path++[dir]
      if pos' == (3, 3)
      then return path'
      else go path' pos'
getNeighbors (x, y) = filter valid . map ((,) =<< step)
  where step 'U' = (x, y-1)
        step 'D' = (x, y+1)
        step 'L' = (x-1, y)
        step 'R' = (x+1, y)
        valid ((x, y),_) = all (`elem` [0..3]) [x, y]
findOpen = map ("UDLR" !!) . findIndices (>= 'b') . take 4 . hash
hash = md5s . Str
