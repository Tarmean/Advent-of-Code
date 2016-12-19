import Data.Sequence
import Prelude hiding (length, splitAt)

a1 = solve $ const 1
a2 = solve $ (`div` 2) . length
solve f = findRemaining . fromList . enumFromTo 1
  where findRemaining = (`index` 0) . until ((==1) . length) (rotate . remove)
        remove = deleteAt =<< f
        rotate = uncurry (flip mappend) . splitAt 1
