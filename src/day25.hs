    module Day25 where
    import Data.List

    main = print (solution - target)
      where Just solution = find (>target) possible
    target = 15*170
    possible = 1 : evenToOdd  (map (* 2) possible)
      where evenToOdd = zipWith (+) (cycle [0, 1])
