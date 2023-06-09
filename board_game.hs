import Data.List
import Control.Monad
import System.IO
import Text.Parsec

-------- Rose tree --------

data Rose a = Rose a [Rose a] deriving (Show)

-- examples of tree
binary_tree = Rose 1 [(Rose 2 [(Rose 3[]),(Rose 3[])]), (Rose 2 [(Rose 3[]),(Rose 3[])])]
linked_tree = Rose 1 [Rose 2 [Rose 3 []]]
nary_tree = Rose 1 [Rose 2 [Rose 3 []], Rose 2 [Rose 3 []], Rose 2 [Rose 3 []]]

size :: Rose a -> Int
size (Rose _ []) = 1
size (Rose a xs) = foldl (\acc val -> size val + acc) 0 xs + 1 

height :: Rose a -> Int
height (Rose _ []) = 1
height (Rose a xs) = foldl (\acc val -> max (height val) acc) 0 xs + 1

leavesCount :: Rose a -> Int
leavesCount (Rose _ []) = 1
leavesCount (Rose a xs) = foldl (\acc val -> leavesCount val + acc) 0 xs

leaves :: Rose a -> [a]
leaves (Rose a []) = [a]
leaves (Rose a xs) = foldl (\acc val -> (leaves val) ++ acc) [] xs

elemsOnDepth :: Rose a -> Int -> [a]
elemsOnDepth (Rose a []) n = if n == 0 then [a] else []
elemsOnDepth (Rose a xs) 0 = [a]
elemsOnDepth (Rose a xs) n = foldl (\acc val -> (elemsOnDepth val (n-1)) ++ acc) [] xs

instance Functor Rose where
    fmap f (Rose n []) = Rose (f n) []
    fmap f (Rose n xs) = Rose (f n) (map (fmap f) xs)

foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc (Rose val []) = f acc val
foldRose f acc (Rose val xs) = f (foldl (\acc a -> foldRose f acc a) acc xs) val

------------------------------------------------------------
-------- State Representation --------

-------- Board game --------
data Board a = Board {turn :: Int, board :: [[a]], dim :: Int}

-------- Move --------
data Move = Move {player :: Int, place :: (Int, Int)}

------------
-------- GameStateOp --------

newtype GameStateOp s a = GameStateOp { runGameStateOp :: s-> (a, s) }

instance Functor (GameStateOp s) where
    fmap f (GameStateOp h) = GameStateOp (\board -> let (a, new_board) = h board in (f a, new_board))


instance Applicative (GameStateOp s) where
    pure x = GameStateOp $ \s -> (x,s)
    (GameStateOp f) <*> (GameStateOp sa) =
     GameStateOp (\board -> let (fn,new_board) = f board
                                (a, s2) = sa new_board
                            in (fn a, s2))

instance Monad (GameStateOp s) where
    return = pure
    GameStateOp h >>= f = GameStateOp $ \board -> let 
                                                    (is_done, new_board) = h board
                                                    (GameStateOp g) = f is_done
                                                    in g new_board

------------
-------- GameStateOpHistory --------

newtype GameStateOpHistory s a = GameStateOpHistory { runGameStateOpHistory ::s-> (a, s) }

instance Functor (GameStateOpHistory s) where
    fmap f (GameStateOpHistory h) = GameStateOpHistory (\board -> let (a, new_board) = h board in (f a, new_board))


instance Applicative (GameStateOpHistory s) where
    pure x = GameStateOpHistory $ \s -> (x,s)
    (GameStateOpHistory f) <*> (GameStateOpHistory sa) =
     GameStateOpHistory (\board -> let (fn,new_board) = f board
                                       (a, s2) = sa new_board
                                    in (fn a, s2))

instance Monad (GameStateOpHistory s) where
    return = pure
    GameStateOpHistory h >>= f = GameStateOpHistory $ \board -> let 
                                                    (is_done, new_board) = h board
                                                    (GameStateOpHistory g) = f is_done
                                                    in g new_board

------------------------------------------------------------
-- examples of board
xo_board = Board  1 [['X', ' ', 'X'],[' ', 'X', ' '], ['O', ' ', 'O']] 3
initial = Board  1 [[' ', ' ', ' '],[' ', ' ', ' '], [' ', ' ', ' ']] 3
lr = Board  1 [['O', 'O', 'O'],['X', 'X', ' '], ['X', ' ', 'O']] 3
up = Board  1 [['X', 'O', 'O'],['X', 'X', ' '], ['X', ' ', 'O']] 3
dig = Board  0 [['X', 'O', ' '],['O', 'X', ' '], [' ', ' ', 'X']] 3
dig2 = Board  0 [['O', 'O', 'X'],['O', 'X', ' '], ['X', ' ', 'X']] 3
ad = Board  1 [['X', 'O', 'X'],['X', 'O', 'O'], ['O', 'X', ' ']] 3
ad2 = Board  0 [['X', ' ', ' '],['X', 'O', 'O'], ['O', 'X', 'X']] 3

-- O is 0, X is 1

-------- show board --------
show_row :: (Show a) => [a] -> String
show_row row = (foldl (\acc elem -> acc ++ (show elem) ++ "|") "|" row) ++ "\n"


show_board :: (Show a) => [[a]] -> String
show_board [[]] = "||"
show_board board = map (\elem -> if elem == '\'' then ' ' else elem) (foldl (\acc row_str -> acc ++ row_str) "" [show_row row | row <- board])

show_pl :: Int -> String
show_pl 0 = "O"
show_pl 1 = "X"

instance (Show a) => Show (Board a) where
    show (Board pl board dim) = " Next move player: "++ (show_pl pl) ++ "\n" ++ show_board board
---------------------------
-------- getting valid moves --------
get_row_valid :: Int -> [Char] -> [(Int, Int)]
get_row_valid rn row = foldl (\acc (cn, elem) -> if elem == ' ' then acc ++ [(rn, cn)] else acc) [] (zip [1,2 ..] row)


-- [(1,"X X"),(2," X "),(3,"O O")]
get_valid_moves :: Board Char -> [(Int, Int)]
get_valid_moves (Board p b dim) = foldl (\acc (idx, row) -> acc ++ (get_row_valid idx row)) [] (zip [1,2 ..] b)

---------------------------
-------- make a move --------
-- given coordinates, makes a move
make_move' :: (Int, Int) -> Board Char -> Board Char
make_move' (rn, cn) (Board p b dim) = Board ((p+1)`mod`2) (take (rn-1) b ++ [take (cn-1) (b !! (rn-1)) ++ [if p == 0 then 'O' else 'X'] ++ drop cn (b !! (rn-1))] ++ drop rn b) dim

make_move :: (Int, Int) -> Board Char -> Board Char
make_move coord (Board p b dim) = if elem coord (get_valid_moves (Board p b dim))  then make_move' coord (Board p b dim) else  error ("Invalid move " ++ show coord)

---------------------------
-------- is the game finished --------

-- check if the game is finished
is_finished :: Board Char -> Bool
is_finished (Board p b dim) = up_down b || left_right b || diagonal b || diagonal' b || (length (get_valid_moves (Board p b dim))) == 0

-- check if the game is finished in left-right direction
left_right :: [[Char]] -> Bool
left_right b = foldl (\acc row -> acc || (foldl (\acc' elem -> acc' && (elem == 'X')) True row) || (foldl (\acc' elem -> acc' && (elem == 'O')) True row)) False b

-- check if the game is finished in up-down direction
up_down :: [[Char]] -> Bool
up_down b = left_right (transpose b)

-- check if the game is finished in first diagonal direction
diagonal :: [[Char]] -> Bool
diagonal b = (foldl (\acc (idx, row) -> (row !! (idx-1) == 'X' && acc)) True (zip [1,2 ..] b)) || (foldl (\acc (idx, row) -> (row !! (idx-1) == 'O' && acc)) True (zip [1,2 ..] b))

-- check if the game is finished in second diagonal direction
diagonal' :: [[Char]] -> Bool
diagonal' b = diagonal (map reverse b)

---------------------------
-------- Rose tree of moves --------

-- get all possible moves
get_all_moves :: Board Char -> Rose (Board Char)
get_all_moves (Board p b dim) = Rose (Board p b dim) (map (\coord -> get_all_moves (make_move coord (Board p b dim))) (get_valid_moves (Board p b dim))) 

game_tree = get_all_moves initial
-- elemsOnDepth (get_all_moves initial) 1

-- In actuality, tic-tac-toe players fill in each of the nine entries with one of only three values: an X, an O, or leave it blank.
-- That's a total of 3*3*3*3*3*3*3*3*3 = 3^9 = 19,683 different ways the 3x3 grid can be filled in.

------------------------------------------------------------
-------- tic-tac-toe with monads --------

apply_move :: (Int, Int) -> GameStateOp (Board Char) Bool
apply_move (x1, y1) = GameStateOp (\board -> let
                                            new_board = make_move coord board
                                            coord = (x1+1, y1+1)
                                            has_done = is_finished board
                                          in 
                                            if (elem coord (get_valid_moves board) && (not has_done)) then (is_finished new_board, new_board) else (has_done , board))


apply_moveh :: (Int, Int) -> GameStateOpHistory [Board Char] Bool
apply_moveh (x1, y1) = GameStateOpHistory (\boards -> let
                                                            current:rest = boards
                                                            new_board = make_move coord current
                                                            coord = (x1+1, y1+1)
                                                            has_done = is_finished current
                                                        in 
                                                            if ((not has_done) && (elem coord (get_valid_moves current))) then (is_finished new_board, new_board:boards) else (has_done, boards))

-- runGameStateOp apply_moves initial
apply_moves :: GameStateOp (Board Char) Bool
apply_moves = do
    apply_move (0,1)
    apply_move (1,0)
    apply_move (0,0)
    apply_move (1,2)
    apply_move (0,2)
    apply_move (1,1)


-- runGameStateOpHistory apply_moves2 [initial]
apply_moves2 :: GameStateOpHistory [Board Char] Bool
apply_moves2 = do
    apply_moveh (0,1)
    apply_moveh (1,0)
    apply_moveh (0,0)

------------------------------------------------------------
-------- Parsing the game from the file --------

-------- Making a list of partialy applied moves --------

-- because in file we don't know exactly how many moves there are
list_moves = [apply_move (0,1), apply_move (1,0), apply_move (0,0)]
list_movesh = [apply_moveh(0,1), apply_moveh (1,0), apply_moveh (0,0)]

-------- Simulation that works with list of moves --------
-- run_simulation list_moves initial_board <using foldl in the background to apply every move
--                                          efectively simulating do block>

                -- list of  partial functions apply_move (x,y) 
run_simulation :: [GameStateOp (Board Char) Bool] -> Board Char -> (Bool, Board Char) 
run_simulation [] board = (is_finished board, board)
run_simulation (x:xs) board = runGameStateOp (foldl (\acc elem -> acc >> elem) x xs) board

run_simulation_h :: [GameStateOpHistory [Board Char] Bool] -> Board Char -> (Bool, [Board Char]) 
run_simulation_h [] board = (is_finished board, [board])
run_simulation_h (x:xs) board = runGameStateOpHistory (foldl (\acc elem -> acc >> elem) x xs) [board]

---------------------------


-- load a game from a file line by line
-- File in format 
--  | X | O |   |
--  |   | X |   |
--  |   |   |   |
-- O (2,2)
-- X (1,0)
-- O (1,2)
-- X (0,2)




-- get_player :: [String] -> Int
-- get_player moves = let
--                         first_move = head moves
--                     in
--                         if (head first_move) == 'X' then 1 else 0

-- get_board :: [String] -> [[Char]]


-- make_board :: [String] -> [String] -> Board Char
-- make_board board moves = Board (get_player moves) (get_board board)

-- main = do  
--     contents <- readFile "game.txt"
--     let 
--         board = [line | line <- (lines contents), elem '|' line]
--         moves = [line | line <- (lines contents), not (elem '|' line)]
--         initial_board = make_board board moves
--     print initial_board