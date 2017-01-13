import Data.List
import Data.Function (on)
import Data.Ord
import Control.Arrow((&&&))
import System.IO

elemCount = map (head &&& length) . group . sort
getExtrema extrema = fst . extrema `on` (comparing snd) . elemCount

solveWith = (. transpose) . map . getExtrema

main = do content <- lines <$> readFile "in06.txt"
          print $ solveWith maximumBy content
          print $ solveWith minimumBy content
