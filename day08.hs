import Data.List
import Data.List.Split
import Text.Parsec.String
import Text.Parsec
import System.Console.ANSI as A
import Control.Concurrent as C

data Command = Col Int Int | Row Int Int | Square Int Int deriving Show
type Field = [[Bool]]

main = do parsed <- parseFromFile parseAll "in08.txt"
          case parsed of
            Left err -> print err
            Right commands -> do
                let steps = process' commands
                mapM_ printMatrix $ steps
                -- let solution = process commands
                -- putStrLn $ "Light count: " ++ show (countLights solution)
                -- let drawn = drawMatrix solution
                print $  ocr . drawMatrix . last $ steps
                -- mapM_ putStrLn drawn

process' :: [Command] -> [Field]
process' = scanl (flip step) start
printMatrix :: Field -> IO ()
printMatrix matrix = do let text = drawMatrix matrix
                        mapM_ putStrLn text
                        putStrLn ""

countLights :: Field -> Int
countLights = length . filter id . concat

start = replicate 6 $ replicate 50 False

process :: [Command] -> Field
process = foldl' (flip step) start

step (Col amount col) = rotateCol col amount
step (Row amount row) = rotateRow row amount
step (Square x y)     = draw x y

rotateRow amount row = replaceBy row (rotateBy amount)
rotateCol amount col = transpose . rotateRow amount col . transpose

rotateBy i ls = recombine $ splitAt  (length ls - i) ls
  where recombine = uncurry . flip $ (++)

replaceBy ::  Int -> (a -> a) -> [a] -> [a]
replaceBy i f ls = start ++ [selected'] ++ end
  where (start, (selected:end)) = splitAt i ls
        selected' = f selected

draw x y matrix = do (i, row) <- zip [0..] matrix
                     return $ do
                       (j, entry) <- zip [0..] row
                       return $ (i < x) && (j < y)  || entry

--------------------------------------------------------------------------------

slices i = map concat . transpose . map (chunksOf i)
ocr = traverse translateLetter . slices 5
  where 
    translateLetter = flip lookup samples
    samples = [(".##..#..#.#....#....#..#..##..", 'C'),
               ("####.#....###..#....#....#....", 'F'),
               ("#....#....#....#....#....####.", 'L'), 
               ("####.#....###..#....#....####.", 'E'),
               ("#..#.#..#.#..#.#..#.#..#..##..", 'U'),
               (".##..#..#.#..#.#..#.#..#..##..", 'O'),
               ("#...##...#.#.#...#....#....#..", 'Y'),
               (".###.#....#.....##.....#.###..", 'S'),
               ("..............................", ' '),
               (".##..#..#.#....#.##.#..#..###.", 'G'),
               ("#####..#....#....#....#....#..", 'T'),
               (".###...#....#....#....#...###.", 'I')]


drawMatrix =  map drawLine
  where drawLine = map drawChar
        drawChar c = if c then '#' else '.'

parseAll :: Parser [Command]
parseAll = sepEndBy parseCommand spaces
parseCommand = try parseCol <|> try parseRow <|> parseSquare <?> "Command"

parseCol = uncurry Col <$> parseSingle "rotate column x=" "by"  
parseRow = uncurry Row <$> parseSingle "rotate row y=" "by"  
parseSquare = uncurry Square <$> parseSingle "rect" "x"  

parseSingle start sep = do string start >> spaces
                           l <- number
                           spaces *> string sep <* spaces
                           r <- number
                           return $ (l, r)

number :: Read a => Num a => Parser a
number = read <$> many1 digit <* spaces
