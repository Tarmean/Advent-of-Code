import Data.List (tails)

step = map ((/=) <$> head <*> last) . windows 3 . ([False] ++) . (++ [False])
solve = flip countSafe . map (== '^')
countSafe n = sum . map (length . filter not) . take n . iterate step
windows n = takeWhile ((==n) . length) . map (take n) . tails
