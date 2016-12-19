import Data.Sequence
import Prelude hiding (length, splitAt)

a1 = solve $ const 1
a2 = solve $ (`div` 2) . length
solve f = go . fromList . enumFromTo 1
  where 
  go ls
    | length ls == 1 = ls `index` 0
    | otherwise      = go $ xs' >< x
    where xs = deleteAt =<< f $ ls
          (x, xs') = splitAt 1 xs
