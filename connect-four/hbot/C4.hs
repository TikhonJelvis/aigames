{-# LANGUAGE DataKinds, ParallelListComp, TypeOperators #-}
module C4 where

import Prelude hiding ((!!))
import qualified Prelude

import Data.Array
import Data.Functor
import Data.List hiding ((!!))
import Data.Maybe
import Data.Modular
import Data.Ord

data Side = First | Second deriving (Show, Eq)

enemy :: Side -> Side
enemy First = Second
enemy Second = First

type Col = ℤ/7
type Row = ℤ/6
type Board  = Array (Int, Int) (Maybe Side)

dispCell :: Maybe Side -> String
dispCell Nothing       = "."
dispCell (Just First)  = "x"
dispCell (Just Second) = "o"

allCells :: Board -> [[Maybe Side]]
allCells board = [[board !! (r, c) | c <- [0..]] | r <- [-1, -2..]]

dispBoard :: Board -> String
dispBoard board = unlines . map (unwords . map dispCell) $ allCells board

toInts (x, y) = (fromIntegral x :: Int, fromIntegral y :: Int)

empty :: Board
empty = listArray ((0, 0), (6, 7)) (repeat Nothing)

setAt :: (Row, Col) -> Side -> Board -> Board
setAt pt side board = board // [(toInts pt, Just side)]

(!!) :: Board -> (Row, Col) -> Maybe Side
board !! pt = board ! toInts pt

play :: Side -> Col -> Board -> Maybe Board
play side col board
  | length colPts == 6 = Nothing
  | null colPts       = Just $ setAt (0, col) side board
  | otherwise         = Just $ setAt (succ . fst $ last colPts, col) side board
  where colPts = takeWhile (isJust . snd) [(row, board !! (row, col)) | row <- [0..]]

row :: Row -> Board -> [Maybe Side]
row n board = [board !! (n, col) | col <- [0..]]

col :: Col -> Board -> [Maybe Side]
col n board = [board !! (row, n) | row <- [0..]]

diag :: Bool -> (Row, Col) -> Board -> [Maybe Side]
diag True (row, col) board  =
  [board !! (r, c) | r <- [row + 1..] | c <- [col + 1..]] ++
  [board !! (r, c) | r <- [row, row - 1..] | c <- [col, col - 1..]]
diag False (row, col) board =
  [board !! (r, c) | r <- [row - 1, row - 2..] | c <- [col + 1..]] ++
  [board !! (r, c) | r <- [row..] | c <- [col, col - 1..]]

emptyCells :: Board -> [(Row, Col)]
emptyCells board = map wrap . filter (isNothing . snd) $ assocs board
  where wrap ((x, y), _) = (fromIntegral x, fromIntegral y)

attacked :: Int -> Side -> Board -> (Row, Col) -> Bool
attacked len side board (r, c) = attackedRow || attackedCol || attackedDiag
  where isEnemy []    = False
        isEnemy (x:_) = x == Just (enemy side)
        check ls = let (l, (_:r)) = splitAt (fromIntegral c) ls
                       lAtt = last $ groupBy (==) l
                       rAtt = head $ groupBy (==) r in
                   isEnemy lAtt && isEnemy rAtt && (length lAtt + length rAtt >= len)
        attackedRow  = check (row r board)
        attackedCol  = check (col c board)
        attackedDiag = check (diag True (r, c) board) && check (diag False (r, c) board)

wins :: Side -> Board -> Bool
wins side board = undefined

score :: Side -> Board -> Double
score side board = 10 * won + actives + 0.1 * centers
  where active s = genericLength $ attacked 3 s board <$> emptyCells board
        actives = active side - active (enemy side)
        centers = genericLength $ filter (== Just side) $ col 3 board
        won = if wins side board then 1 else 0

argmax :: Ord o => (a -> o) -> [a] -> a
argmax _  [] = error "Cannot get argmax of empty list!"
argmax fn ls = fst . maximumBy (comparing snd) $ zip ls (fn <$> ls)

argmin :: Ord o => (a -> o) -> [a] -> a
argmin _  [] = error "Cannot get argmax of empty list!"
argmin fn ls = fst . minimumBy (comparing snd) $ zip ls (fn <$> ls)

possible :: Side -> Board -> [(Col, Board)]
possible side board =
  [(c, fromJust res) | c <- [0..], let res = play side c board, isJust res]

-- final :: Side -> Board -> (Col, Board)
-- final side board = argmax (score side . snd) $ possible side board

depth = 4

minimax :: Int -> Side -> Board -> Double
minimax depth side board = go depth side board
  where go 0 s b = score s b
        go n s b = maximum $ go (n - 1) (enemy s) . snd <$> possible s b

decide :: Side -> Board -> Col
decide side board = fst . argmax (minimax depth side . snd) $ possible side board
