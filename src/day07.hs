import Data.List 
import Data.List.Split
import Control.Applicative (liftA2)

parseLine ls = parseOuter ls ([""], [])
  where parseOuter ('[':xs) (l, r) = parseInner xs (l, "":r)
        parseOuter (x:xs) ((l:ls), r) = parseOuter xs ((x:l):ls, r)
        parseOuter [] result = result
        parseInner (']':xs) (l, r) = parseOuter xs ("":l, r)
        parseInner (x:xs) (l, (r:rs)) = parseInner xs (l, (x:r):rs)
        parseInner [] result = result

windows i = takeWhile ((== i) . length) . map (take i) . tails
mirrored = (filter isMirror .) . windows
  where isMirror = liftA2 (&&) ((==) =<< reverse) ((> 1) . length . nub)
getABBA = mirrored 4
getABA = mirrored 3

checkABBA = uncurry $ (. lacksABBA) . (&&) . (not . lacksABBA)
  where lacksABBA = null . (getABBA =<<)
checkSSL (l, r) = not . null $ [a |a <- outer, b <- inner, isInsideOut a b]
  where outer = getABA =<< l
        inner = getABA =<< r
        isInsideOut [a, b,c] [d, e, f] = a == e && d == b

countLines f = length . filter f  . map parseLine
countABA = countLines checkSSL
countABBA = countLines checkABBA

main = do content <- lines <$> readFile "in07.txt"
          print $ countABBA content
          print $ countABA content
