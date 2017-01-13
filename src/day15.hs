import System.IO

main = print . firstValid . parseAll =<< readFile "in15.txt"

firstValid discs = head [time | time <- [0..], all (isValid time) discs]
  where isValid time (slots, offset) = (offset + time) `mod` slots == 0

parseAll = map parseLine . lines
parseLine ln = (slots, offset)
  where tokens = words ln
        position = read . init . last $ tokens
        slots = read $ tokens !! 3
        idx = read . tail $ tokens !! 1
        offset = idx + position
