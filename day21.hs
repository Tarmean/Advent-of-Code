import Data.List as L
import Data.Maybe (fromJust)
swapPos x y ls = step <$> zip [0..] ls
  where a = ls!!x
        b = ls!!y
        step (i, c)
          | i == x = b
          | i == y = a
          | otherwise = c
swapLetter x y = map step
  where step c
          | c == x = y
          | c == y = x
          | otherwise = c

rotateN i ls = uncurry (flip (++)) $ splitAt n ls
  where n = (l+i) `mod` l
        l = length ls
  
rotateX c ls = rotateN amount ls
  where idx = fromJust $ elemIndex c ls
        amount = idx + 2 + if idx >= 4 then 1 else 0

reversePart x y ls = l ++ m ++ r
  where l = take x ls
        m = reverse . take (y-x+1) . drop x $ ls
        r = drop (x+y+1) ls


movePos x y ls = go $ zip [0..] ls
  where go ((i,c):r)
          | i == x = go r
          | i +1 == length ls && i == y = c:s:go r
          | i == y = s:c:go r
          | otherwise = c:go r
        go [] = ""
        s = ls !! x

main = process "abcde" <$> readFile "in21.txt"
process start ls = scanr (parse . words) start . reverse $ lines ls
parse ["reverse", "positions", a, "through", b] =  reversePart (read a) (read b)
parse ["move", "position", a, "to", "position", b] =  movePos (read a) (read b)
parse ["swap", "position", a, "with", "position", b] =  swapPos (read a) (read b)
parse ["swap", "letter", a, "with", "letter", b] =  swapLetter (head a) (head b)
parse ["rotate", "based", "on", "position", "of", "letter", b] =  rotateX  (head b)
parse ["rotate", dir,  a, "step"] = rotateN  (direction dir * read a)
  where direction "left" = 1
        direction "right" = -1

