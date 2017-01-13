import qualified Data.Map as M
import Data.List (sortBy, break, isInfixOf)
import Data.Char (isAlpha, isNumber)
import Data.Function (on)
import Data.Ord
import System.IO

toCheckSum = take 5 . sortByCount . toCounts
  where toCounts = M.toList . M.fromListWith (+) . map (flip (,) 1)
        sortByCount = map fst . sortBy (comparing $ Down . snd)

checkLine line = checkSum == checkSum'
  where (lhs, rhs) = break (== '[') line
        checkSum' = toCheckSum . clean $ lhs
        checkSum = clean rhs
        clean = filter isAlpha

decrypt = shift =<< toNum
shift = map . shiftLetter
shiftLetter times c
  | isAlpha c  = c'
  | isNumber c = c
  | otherwise  = ' '
  where base = fromEnum 'a'
        shifted = fromEnum c - base + times
        wrapped = shifted `mod` 26
        c' = toEnum $ wrapped + base

toNum = read . filter isNumber
sumEntries = sum . map toNum

sumValidEntries =  sumEntries . filter checkLine
shiftValidEntries = map decrypt . filter checkLine
searchNorth = filter (isInfixOf "north") . shiftValidEntries

main = do
    handle <- openFile "in04.txt" ReadMode
    contents <- hGetContents handle
    let allLines = lines contents
    print $ sumValidEntries allLines
    traverse print $ searchNorth allLines
    hClose handle
