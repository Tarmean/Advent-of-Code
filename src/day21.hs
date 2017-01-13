module Day21 where
import Prelude  as P hiding (splitAt, length)
import Data.Monoid
import Data.Sequence as S
import Debug.Trace


changeTo = adjust . const
swapPos x y s = s''
  where a = s `index` x
        b = s `index` y
        s' = changeTo b x s
        s'' = changeTo a y s'
swapLetter x y = fmap step
  where step c
         | x == c = y
         | y == c = x
         | otherwise = c
rotate a s = r <> l
  where (l, r) = splitAt b s
        b =  mod (-a) (length s)
rotateOn c s = rotate pos' s
  where Just pos = succ <$> elemIndexL c s
        pos' = if pos > 4 then succ pos else pos
        
reverseRange from to s = l <> S.reverse m <> r
  where
    (l, rest) = splitAt from s
    (m, r) = splitAt (to - from + 1) rest
move from to s = insertAt to entry s'
  where entry = index s from
        s' = deleteAt  from s

swapPos' = flip swapPos
swapLetter' x y = swapLetter x y
rotate' i = rotate (-i)
rotateOn' c s = until ((== s) . rotateOn c) (rotate 1) s
reverseRange' = reverseRange
move' from to = move to from

parse' ["swap", "position", x, "with", "position", y] = swapPos' (read x) (read y)
parse' ["swap", "letter", x, "with", "letter", y] = swapLetter' (head x) (head y)
parse' ["rotate", "left", x, _] = rotate' $ - read x
parse' ["rotate", "right", x, _] = rotate' $ read x
parse' ["rotate", "based", "on", "position", "of", "letter", x] = rotateOn' (head x)
parse' ["reverse", "positions", x, "through", y] = reverseRange' (read x) (read y)
parse' ["move", "position", x, "to", "position", y] = move' (read x) (read y)

parse ["swap", "position", x, "with", "position", y] = swapPos (read x) (read y)
parse ["swap", "letter", x, "with", "letter", y] = swapLetter (head x) (head y)
parse ["rotate", "left", x, _] = rotate $ - read x
parse ["rotate", "right", x, _] = rotate $ read x
parse ["rotate", "based", "on", "position", "of", "letter", x] = rotateOn (head x)
parse ["reverse", "positions", x, "through", y] = reverseRange (read x) (read y)
parse ["move", "position", x, "to", "position", y] = move (read x) (read y)


process :: a -> [a->a] -> a
process = P.foldl (flip ($))
processI :: a -> [a->a] -> [a]
processI = P.scanl (flip ($))

parseFile = map (parse . words) . lines
parseFile' = map (parse' . words) . P.reverse . lines


main =  do
  instructions <- parseFile <$> readFile "in21.txt"
  print $ process (fromList "abcdefgh") instructions
  instructions' <- parseFile' <$> readFile "in21.txt"
  print $ process (fromList "fbgdceah") instructions'
