{-# Language PackageImports #-}
import qualified Data.ByteString.Char8 as B
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (tails)
import Data.ByteArray.Encoding
import "cryptonite" Crypto.Hash
import Data.Monoid
import Control.Monad

hashes = flip flip [0..] . (ap zip . map .: md5)
md5 salt n i = iterateN n hash input
  where hash = convertToBase Base16 . hashWith MD5
        input = salt <> B.pack (show i)

findSolutions = mapMaybe single . tails .: hashes
  where
     single ((i, x):xs) = listToMaybe [i | window <- windowsWithAnyChar 3 x,
                                                     searchWindow5 window $ map snd xs]
     searchWindow5 (c:_) = any (not . null . windowsWithSpecificChar 5 c) . take 1000

windowsWithSpecificChar m c =  filter (all (== c)) . windows m
windowsWithAnyChar m = filter identical . windows m
  where identical (x:xs) = all (==x) xs
windows m = slices . B.unpack
  where slices = takeWhile ((== m) . length) . map (take m) . tails
iterateN n f = (!!n) . iterate f 
infixl 8 .:
(.:) = (.).(.)
