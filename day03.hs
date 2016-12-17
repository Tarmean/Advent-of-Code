import System.IO
import Data.List (sort, transpose)
import Data.List.Split 
import Control.Monad ((<=<))

parseLine = map read . filter (not . null) . splitOn " "
parseFile = map parseLine

countValid = length . filter isValid

isValid = check . sort
  where check [a,b,c] = a + b > c

colsToRows = transpose <=< chunksOf 3

countRows = countValid . parseFile
countColumns = countValid . colsToRows . parseFile

main = do
    handle <- openFile "in03.txt" ReadMode
    contents <- hGetContents handle
    let allLines = lines contents
    print $ countRows allLines
    print $ countColumns allLines
    hClose handle
