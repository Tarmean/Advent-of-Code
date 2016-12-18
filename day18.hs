import Data.List (tails)

step = map ((/=) <$> head <*> last) . windows 3 . ([False] ++) . (++ [False])
countDots n = length . filter not . concat . take n . iterate step
solve = flip countDots . map (== '^')

windows n = takeWhile ((==n) . length) . map (take n) . tails
