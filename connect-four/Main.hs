module Main where

import C4

import Data.Maybe

main = go Second empty

go side board =
  do col <- readLn
     let Just board' = play (enemy side) col board
     putStr $ dispBoard board'
     let move = decide side board'
         board'' = fromJust $ play side move board'
     print move
     putStr $ dispBoard board''
     go side board''
              
