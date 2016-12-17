import Data.Map as M
import Control.Monad.State
import Data.Char
import System.IO
import Data.Maybe (fromMaybe)

main = process . parseAll <$> readFile "in12.txt" >>= print

process :: [Op] -> Definitions
process commands = evalState go (0, M.empty)
  where 
    go = do
      (i, m) <- get
      if i >= length commands
      then return m
      else do
        put $ case commands !! i of
          (Copy from to)   -> (succ i, M.insert to (loadVal from m) m)
          (Inc reg)        -> (succ i, M.adjust succ reg m)
          (Dec reg)        -> (succ i, M.adjust pred reg m)
          (Jump cond delta) -> let i' = if 0 /= loadVal cond m then i+delta else i+1
                               in (i', m)
        go

type Definitions = M.Map Char Int
data Value = Register Char | Constant Int deriving Show
data Op = Copy Value Char
          | Jump Value Int
          | Inc Char
          | Dec Char deriving Show

loadVal (Register r) state = fromMaybe 0 $ M.lookup r state
loadVal (Constant const) _ = const
            

parseAll = fmap parse . lines 
parse = go . words
  where go ["cpy", a, b] = Copy (pVal a) (pReg b)
        go ["jnz", a, b] = Jump (pVal a) (pConst b)
        go ["inc", a]    = Inc (pReg a)
        go ["dec", a]    = Dec (pReg a)

        pReg = head
        pConst = read
        pVal i = if any isAlpha i then Register (pReg i) else Constant (pConst i)
