import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

main = print =<< parseFromFile (parser True) "in09.txt"

parser recursive = sum <$> many1 (repeated recursive <|> plain)
plain = length <$> (many1 $ noneOf "(")
repeated recursive = do (len, times) <- repeatMarker
                        repeated <- count len anyChar
                        return $ times * (flatten repeated)
  where flatten
         | recursive = either (const 0) id . parse (parser recursive) ""
         | otherwise = length
                
repeatMarker = between (char '(') (char ')') $ (,) <$> (num <* char 'x') <*> num
num = read <$> many1 digit
