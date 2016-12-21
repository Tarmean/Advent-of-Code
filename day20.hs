import Data.List.Split (splitOn)

parse :: String -> [(Int, Int)]
parse = map parseLine . lines
parseLine line = (read a, read b)
  where [a, b] = splitOn "-" line

firstValid cur ((a, b):rest)
  | cur+1 < a = cur+1
  | otherwise = firstValid (max b cur) rest

countValid cur ((a, b):rest) = max 0 (a - cur-1) + countValid (max b cur) rest
countValid cur [] = 2^32-1 - cur

main = ((,) <$> countValid 0 <*> firstValid 0) . parse <$> readFile "in20.txt"
