import Data.List.Split (chunksOf)

solve = map toRep . checksum .: (. map (=='1')) . fillDisk
  where toRep True = '1'
        toRep False = '0'
fillDisk size = take size . until ((>=size) . length) step
  where step a = a ++ [False] ++ (map not . reverse $ a)
checksum = until (odd . length) step
  where step = map (foldl1 (==)) . chunksOf 2

infixl 8 .:
(.:) = (.).(.)
