import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as M
import qualified Data.List as L

type BotMap = M.Map Id Robot
data Robot = Robot Id Id [Value] deriving Show
type Value = Int
type Idx = Int
data Id = Bot Idx | Output Idx | None deriving (Show, Eq, Ord)
data Entry = In Id Value | Out Id Id Id deriving Show

pFormat :: Parser [Entry]
pFormat = sepBy (pInput <|> pOutput) spaces

pInput = do string "value" >> spaces
            val <- integer
            string "goes to bot" >> spaces
            id <- integer
            return $ In (Bot id) val
pOutput = do string "bot" >> spaces
             id <- integer
             string "gives low to" >> spaces
             idTypeLow <- pIdType
             low <- idTypeLow <$> integer
             string "and high to" >> spaces
             idTypeHigh <- pIdType
             high <- idTypeHigh <$> integer
             return $ Out (Bot id) low high
pIdType :: Parser (Int -> Id)
pIdType = (string "bot" >> spaces >> return Bot) <|>
          (string "output" >> spaces >> return Output)
integer :: Parser Int
integer = read <$> many1 digit <* spaces


makeBots :: [Entry] -> BotMap
makeBots = foldr step start
 where 
   start = M.fromList $ toEntry <$> (Output <$> [0..20])
   toEntry = flip (,) $ Robot None None []
   step (Out idx l h) = M.insert idx (Robot l h [])
   step _ = id
insertValue :: Entry -> BotMap -> BotMap
insertValue (In idx value) botMap = insertRec v' m'
  where 
    (Just (Robot l h v)) = M.lookup idx botMap
    v' = L.sort $ value:v
    m' = M.insert idx (Robot l h v') botMap
    insertRec [low, high] = insertValue (In l low) . insertValue (In h high)
    insertRec _           = id
insertValue _ botMap = botMap
insertAll :: BotMap -> [Entry] -> BotMap
insertAll = foldr insertValue
        

        
main = do r <- parseFromFile pFormat "in10.txt"
          case r of
            Right entries -> do
              let bots = makeBots entries
              let bots'  = insertAll bots entries
              let bots_f = M.toList bots'
              mapM_ print bots_f
