{-# Language ViewPatterns #-}
module Day23 where
import Data.Map as M
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Char

type Definitions = M.Map Char Int
type Program = ([Op], Int, Definitions)
data Value = Register Char | Constant Int deriving Show
data Op = Copy Value Value
          | Jump Value Value
          | Mult Value Value Value
          | Noop
          | Togl Value
          | Inc Value
          | Dec Value deriving Show

process commands = evalState step (commands, 0, M.empty)
main = process . parseAll <$> readFile "in12.txt" >>= print

step :: State Program Definitions
step = do
  (ops, idx, defs) <- get
  if idx >= length ops
  then return defs
  else do
    case ops !! idx of
      (Copy (load defs -> Just value) (Register reg)) -> _3 . at reg .= Just value
      (Jump (load defs -> Just cond) (load defs -> Just dist)) -> when (cond > 0) $ _2 += dist-1
      (Mult (load defs -> Just a) (load defs -> Just b) (Register r)) -> _3 . ix r .= a * b
      (Togl (load defs -> Just i)) -> _1 . ix (i+idx) %= toggle
      (Inc (Register r)) -> _3 . ix r += 1
      (Dec (Register r)) -> _3 . ix r -= 1
      _ -> return ()
    _2 <+= 1
    step

toggle (Copy a b) = Jump a b
toggle (Jump a b) = Copy a b
toggle (Inc a) = Dec a
toggle (Dec a) = Inc a
toggle (Togl a) = Inc a

load defs (Register c) = M.lookup c defs
load _ (Constant i) = Just i

parseAll = fmap parse . lines 
parse = go . words
  where go ["cpy", a, b] = Copy (pVal a) (pVal b)
        go ["jnz", a, b] = Jump (pVal a) (pVal b)
        go ["inc", a]    = Inc (pVal a)
        go ["dec", a]    = Dec (pVal a)
        go ["tgl", a]    = Togl (pVal a)
        go ["mult", a, b, c] = Mult (pVal a) (pVal b) (pVal c)
        go [] = Noop
        pReg = head
        pConst = read
        pVal i = if any isAlpha i then Register (pReg i) else Constant (pConst i)
