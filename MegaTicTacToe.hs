module Main where

import System.Random
import Text.Read (readMaybe)
import Data.Array.IO
import Control.Monad

-- Structures

data Cell = E | X | O | T
  deriving Eq

instance Show Cell where
  show E = " "
  show X = "X"
  show O = "O"
  show T = "T"

showTTT :: [Cell] -> String
showTTT [a,b,c,d,e,f,g,h,i] =
  show a ++ show b ++ show c ++ "\n" ++ 
  show d ++ show e ++ show f ++ "\n" ++ 
  show g ++ show h ++ show i

showTTTline :: [Cell] -> Int -> String
showTTTline [a,b,c,_,_,_,_,_,_] 0 = show a ++ show b ++ show c
showTTTline [_,_,_,d,e,f,_,_,_] 1 = show d ++ show e ++ show f
showTTTline [_,_,_,_,_,_,g,h,i] 2 = show g ++ show h ++ show i 

data Board = Board Cell Int Int [Int] [Cell] [[Cell]]
  deriving Eq
  
instance Show Board where
  show (Board turn next _ _ summ [a,b,c,d,e,f,g,h,i]) = 
    (showTTTline a 0) ++ "|" ++ (showTTTline b 0) ++ "|" ++ (showTTTline c 0) ++ "\n" ++
    (showTTTline a 1) ++ "|" ++ (showTTTline b 1) ++ "|" ++ (showTTTline c 1) ++ "\n" ++
    (showTTTline a 2) ++ "|" ++ (showTTTline b 2) ++ "|" ++ (showTTTline c 2) ++ "\n" ++
    "-----------\n"++
    (showTTTline d 0) ++ "|" ++ (showTTTline e 0) ++ "|" ++ (showTTTline f 0) ++ "\n" ++
    (showTTTline d 1) ++ "|" ++ (showTTTline e 1) ++ "|" ++ (showTTTline f 1) ++ "\n" ++
    (showTTTline d 2) ++ "|" ++ (showTTTline e 2) ++ "|" ++ (showTTTline f 2) ++ "\n" ++
    "-----------\n" ++
    (showTTTline g 0) ++ "|" ++ (showTTTline h 0) ++ "|" ++ (showTTTline i 0) ++ "\n" ++
    (showTTTline g 1) ++ "|" ++ (showTTTline h 1) ++ "|" ++ (showTTTline i 1) ++ "\n" ++
    (showTTTline g 2) ++ "|" ++ (showTTTline h 2) ++ "|" ++ (showTTTline i 2) ++ "\n\n" ++
    showTTT summ ++ "\n" ++ showTurn turn next

showTurn :: Cell -> Int -> String
showTurn t 9 = "Turn of " ++ show t ++ " in any area"
showTurn t n = "Turn of " ++ show t ++ " in area " ++ show n

----------------------------------------------------------------------------------------------

-- Game Logic

emptyTTT :: [Cell]
emptyTTT = replicate 9 E

resultTTT :: [Cell] -> Cell
resultTTT [a,b,c,d,e,f,g,h,i] =  if 
  (a == X) && (b == X) && (c == X) || 
  (d == X) && (e == X) && (f == X) || 
  (g == X) && (h == X) && (i == X) ||
  (a == X) && (d == X) && (g == X) ||
  (b == X) && (e == X) && (h == X) ||
  (c == X) && (f == X) && (i == X) ||
  (a == X) && (e == X) && (i == X) ||
  (c == X) && (e == X) && (g == X) then X else if
  (a == O) && (b == O) && (c == O) || 
  (d == O) && (e == O) && (f == O) || 
  (g == O) && (h == O) && (i == O) ||
  (a == O) && (d == O) && (g == O) ||
  (b == O) && (e == O) && (h == O) ||
  (c == O) && (f == O) && (i == O) ||
  (a == O) && (e == O) && (i == O) ||
  (c == O) && (e == O) && (g == O) then O else if
  (a == E) || (b == E) || (c == E) || (d == E) || (e == E) || (f == E) || (g == E) || (h == E) || (i == E) then E else T
  
newBoard :: Board
newBoard = Board X 9 0 (replicate 9 0) emptyTTT (replicate 9 emptyTTT)

turn :: Board -> Cell
turn (Board t _ _ _ _ _) = t

area :: Board -> Int
area (Board _ a _ _ _ _) = a

count :: Board -> Int
count (Board _ _ c _ _ _) = c

counts :: Board -> [Int]
counts (Board _ _ _ cs _ _) = cs

summary :: Board -> [Cell]
summary (Board _ _ _ _ s _) = s

ttts :: Board -> [[Cell]]
ttts (Board _ _ _ _ _ ttt) = ttt

------------------------------------------------------------------------------------------------

-- Move Logic

validMove :: Board -> Int -> Int -> Bool
validMove (Board turn area _ _ summ ttts) a c = 
  ((area == a) || (area == 9)) &&
  (a >= 0) && (a < 9) && (c >= 0) && (c < 9) && 
  ((ttts !! a) !! c == E) && (summ !! a == E)
  
replaceInd :: [a] -> a -> Int -> [a]
replaceInd l x n = let (f,_:s) = splitAt n l in f ++ x:s

completesTTT :: [Cell] -> Cell -> Int -> Bool
completesTTT [a,b,c,d,e,f,g,h,i] s p =
  [(b == s && c == s) || (d == s && g == s) || (e == s && i == s),
   (a == s && c == s) || (e == s && h == s),
   (a == s && b == s) || (f == s && i == s) || (e == s && g == s),
   (e == s && f == s) || (a == s && g == s),
   (b == s && h == s) || (d == s && f == s) || (a == s && i == s) || (c == s && g == s),
   (d == s && e == s) || (c == s && i == s),
   (a == s && d == s) || (h == s && i == s) || (e == s && c == s),
   (g == s && i == s) || (b == s && e == s),
   (c == s && f == s) || (h == s && g == s) || (e == s && a == s)] !! p
   
other :: Cell -> Cell
other X = O
other O = X

move :: Board -> Int -> Int -> Board
move (Board turn area count counts summ ttts) a c =
  let loccount = (counts !! a) + 1
      newcounts = replaceInd counts loccount a
      ttta = (ttts !! a)
      newttt = replaceInd ttta turn c
      simb = if completesTTT ttta turn c then turn else if loccount == 9 then T else E
      newsumm = if simb == E then summ else replaceInd summ simb a
      newcount = if simb == E then count else count + 1
      newturn = if  (simb /= E) && completesTTT summ simb a then E else if newcount == 9 then T else other turn
      newarea = if newsumm !! c == E then c else 9 
  in
  Board newturn newarea newcount newcounts newsumm (replaceInd ttts newttt a)
  
listValids :: Board -> [(Int,Int)]
listValids b = if area b == 9 
  then filter (\(a,c) -> validMove b a c) [(a, c) | a <- [0..8], c <- [0..8]]
  else zip (repeat (area b)) (filter (validMove b (area b)) [0..8])
  
------------------------------------------------------------------------------------------------

-- minmax

data GameTree = Node Board [(Int,Int,GameTree)]
  deriving (Eq, Show)
  
gameTree :: Board -> GameTree
gameTree b = if (turn b == E) || (turn b == T) 
  then Node b []
  else Node b (map (\(a,c) -> (a,c, gameTree (move b a c))) (listValids b))
  
trim :: GameTree -> Int -> GameTree
trim (Node board _) 0 = Node board []
trim (Node board list) n = Node board (map (\(a,c,b) -> (a,c, trim b (n-1))) list)

chanceTTT :: [Cell] -> Cell -> Int
chanceTTT [a,b,c,d,e,f,g,h,i] p = 
  let 
    either = \x -> (x == E) || (x == p)
    value = \x -> if (x == p) then 1 else 0
    line = \x y z -> if either x && either y && either z 
      then value x + value y + value z
      else 0 
  in
    line a b c + 
    line d e f + 
    line g h i +
    line a d g + 
    line b e h + 
    line c f i +
    line a e i + 
    line c e g

heuristic :: Board -> Cell -> Int
heuristic board p = case (turn board) of
  T -> 0
  E -> if (resultTTT (summary board) == p) 
    then 100
    else -100
  _ -> chanceTTT (summary board) p - chanceTTT (summary board) (other p)


third (_,_,a) = a

maxt ((a,b,c):l) = maxtAc (a,b,c) l
maxtAc (a,b,c) [] = (a,b,c)
maxtAc (a,b,c) ((d,e,f):l) = maxtAc (if c < f then (d,e,f) else (a,b,c)) l

mint ((a,b,c):l) = mintAc (a,b,c) l
mintAc (a,b,c) [] = (a,b,c)
mintAc (a,b,c) ((d,e,f):l) = mintAc (if c > f then (d,e,f) else (a,b,c)) l

minmax :: GameTree -> Cell -> (Int,Int,Int)
minmax (Node b []) p = (-1,-1,heuristic b p)
minmax (Node b l) p = let res = map (\(a,c,g) -> (a,c, third (minmax g p))) l in
  if (turn b == p) then maxt res else mint res

  
------------------------------------------------------------------------------------------------

-- random minmax

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

minmaxRand :: GameTree -> Cell -> IO (Int,Int,Int)
minmaxRand (Node b l) c = do
  perm <- shuffle l
  return (minmax (Node b perm) c)


----------------------------------------------------------------------------------------------

-- monads

moveIO :: Board -> IO Board
moveIO board = do
  putStrLn (show board)
  putStrLn (show (listValids board))
  areaStr <- getLine
  cellStr <- getLine
  case (readMaybe areaStr, readMaybe cellStr) of
    (Just area, Just cell) ->
      if validMove board area cell 
      then return (move board area cell) 
      else do
        putStrLn "Invalid Move"
        moveIO board
    _ -> do
      putStrLn "Invalid input"
      moveIO board

playHH :: Board -> IO Board
playHH board = do
  b2 <- moveIO board
  if (turn b2 == E) || (turn b2 == T)
  then return b2
  else playHH b2

moveIA :: Board -> Int -> IO Board
moveIA b lvl = do
  (a,c,v) <- minmaxRand (trim (gameTree b) lvl) (turn b)
  return (move b a c)
  
playHM :: Board -> IO Board
playHM board = do
  b2 <- if (turn board == X) then moveIO board else moveIA board 4
  if (turn b2 == E) || (turn b2 == T)
  then return b2
  else playHM b2
  
playMM :: Board -> IO Board
playMM board = do
  b2 <- if (turn board == X) then moveIA board 3 else moveIA board 3
  if (turn b2 == E) || (turn b2 == T)
  then return b2
  else playMM b2
  
main :: IO ()
main = do
  let board = newBoard
  bfinal <- playMM board
  putStrLn (show bfinal)
  if turn bfinal == T
  then putStrLn "The game ended in a Tie"
  else putStrLn ("The winner is " ++ show (resultTTT (summary bfinal)))
