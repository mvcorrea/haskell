module Sudoku_v2 where
-- Marcelo Veras Correa - 26958
-- Porgramacao Funcional 2014-15SI

import Data.List
import Data.Char
import Debug.Trace
import System.Time

type Value = Char
type Line a = [ a ]
type Grid a = [ Line a ]
type Game = Grid Value

game = s6  -- game to play  <<--- CHANGE HERE!!!!

s1,s2,s3,s4,s5,s6 :: Game
-- do prof..
s1 = ["2....1.38","........5",".7...6...",".......13",".981..257","31....8..","9..8...2.",".5..69784","4..25...."] -- prof easy
s2 = [".1.42...5","..2.71.39",".......4.","2.71....6","....4....","6....74.3",".7.......","12.73.5..","3...82.7."] -- prof easy
s3 = [".9.7..86.",".31..5.2.","8.6......","..7.5...6","...3.7...","5...1.7..","......1.9",".2.6..35.",".54..8.7."] -- prof Diabolico
s4 = [".98......","....7....","....15...","1........","...2....9","...9.6.82",".......3.","5.1......","...4...2."] -- prof 1 sol
s5 = ["2....1.38","........5",".7...6...",".......13",".981..257","31....8..","9..8...2.",".5..69784","4..25...."] -- net easy
s6 = ["..4......",".7.2..59.",".6..35..1","..9....7.","..6...2..",".1....9..","5..49..1.",".42..3.6.","......4.."]



bside, lsize, llen :: Int
llen = (length game)      -- board side
bside = (floor . sqrt . fromIntegral $ llen)    -- box side
lsize = foldr (+) 0 [1..llen]  -- max sum of line elements

elArr :: String
elArr = concat . map show $ [1..llen] -- string with elements

-- lines
lin' :: Grid a -> [Line a]
lin' a = a

-- columns
col' :: Grid a -> [Line a]
col' a = transpose a

-- boxes
box' :: Grid a -> [Line a]
box' a = map concat . concat . map col' . split bside . splitBox bside $ a

-- original:    ["..2.","1.4.","...4",".4.2"]
-- splitBox:    [["..","2."],["1.","4."],["..",".4"],[".4",".2"]]
-- split:       [[["..","2."],["1.","4."]],[["..",".4"],[".4",".2"]]]
-- map col':    [[["..","1."],["2.","4."]],[["..",".4"],[".4",".2"]]]
-- concat:     2[["..","1."],["2.","4."],["..",".4"],[".4",".2"]]
-- map concat:  ["..1.","2.4.","...4",".4.2"]


-- game valid!  (1st: every line must have lsize "validLineLength") &
--              (2nd: every element must be in range [1..lsize] "validLineRange") &
--              (3rd: every element in line must not repeat itself "validLineElems")
--valid' :: Game -> Bool
valid' xs = (and . validGrid . lin' $ xs) &&
            (and . validGrid . col' $ xs) &&
            (and . validGrid . box' $ xs)


-- solution ----------------------------------------------


type Choices = [Value]

-- if a char is "dot" send all options "elArr" return number otherwise
choice :: Char -> [Char]
choice x | x == '.' = elArr
         | otherwise = [x]

-- 20150417 -- begin

-- remove dots from list to compare (valid values that should not repeat)
-- lstValues ".1.2.3" -> "123"
lstValues :: [Char] -> [Char]
lstValues xs | xs == [] = []
             | otherwise = filter (/='.') xs

-- remove duplicates from lines
-- remDups "12" "13256" -> 356
-- recebe a lista que quero retirar e a lista original
remDups :: [Char] -> [Char] -> [Char]
remDups _  (y:[])  = [y] 
remDups [] ys      = ys
remDups (x:xs) ys  = remDups xs (filter (/=x) ys) 


-- let xx = "2....1.38"
-- remDups (lstValues xx) (choice . head $ xx)       -> "2"
-- remDups (lstValues xx) (choice . head. tail $ xx) -> "45679"


-- process position
-- procPos (lstValues xx) xx -> ["2","45679","45679","45679","45679","1","45679","3","8"]
procPos :: [Char] -> [Char] -> [[Char]]
procPos _ []          = []
procPos zs xss@(x:xs) = (remDups zs (choice x)) : procPos zs xs 


-- optimized list of choices
choices1 :: Game -> Grid Choices
choices1 [] = []
choices1 (x:xs) = procPos (lstValues x) x : choices1 xs


-- 20150417 -- end


-- list of choices
choices :: Game -> Grid Choices
choices =  map (map choice)

-- cartesian product
cprd :: [[a]] -> [[a]]
cprd [] =  [[]]
cprd (xs:xss) = [y:ys | y <- xs, ys <- cprd xss]

-- all possibilities
collapse :: Grid [a] -> [Grid a]
collapse = cprd . map cprd


-- 2015-02-12 -- begin
prune :: Grid Choices -> Grid Choices
prune =  pruneBy box' . pruneBy col' . pruneBy lin'
         where pruneBy f = f . map reduce . f
                               
reduce :: Line Choices -> Line Choices
reduce xss = [xs `minus` singles | xs <- xss]
             where singles = concat (filter single xss)
                               
minus :: Choices -> Choices -> Choices
xs `minus` ys =  if single xs then xs else xs \\ ys

single :: Line a -> Bool
single [_] =  True
single _ =  False

-- ASK!
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
                  where x' = f x

------------------------------------------------------
-- ASK SOLVE4
search :: Grid Choices -> [Game]
search m | blocked m    =  []
         | complete m   =  collapse m
         | otherwise    =  [g | m' <- expand m, g  <- search (prune m')]

blocked :: Grid Choices -> Bool
blocked m = void m || not (safe m)

-- verufy the game
safe :: Grid Choices -> Bool
safe cm =  all consistent (lin' cm) &&
           all consistent (col' cm) &&
           all consistent (box' cm)

-- best way to verify box/col/lin
consistent :: Line Choices -> Bool
consistent =  nodups . concat . filter single

-- verify item duplication
-- verufy the game
nodups :: Eq a =>  [a] -> Bool
nodups [] =  True
nodups (x:xs) = not (elem x xs) && nodups xs

void :: Grid Choices -> Bool
void =  any (any null)

complete :: Grid Choices -> Bool
complete =  all (all single)

expand :: Grid Choices -> [Grid Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
        where
            (rows1,row:rows2) = break (any (not . single)) m
            (row1,cs:row2)    = break (not . single) row

-----------------------------------------------------

-- 2015-02-12 -- end

-- filter only the valid grids

-- solve1 a minha solucao
solve1 :: Game -> [Game]
solve1 =  filter valid' . collapse . choices

solve2 :: Game -> [Game]
solve2 =  filter valid' . collapse . prune . choices

solve3 :: Game -> [Game]
solve3 =  filter valid' . collapse . fix prune . choices1

solve4 :: Game -> [Game]
solve4 =  search . prune . choices

-- choice '.' -> "1234" && choice 5 -> "5"
-- choices ["..",".2"] -> [["1234","1234"],["1234","2"]]
-- cprdu [["1234","1234"],["1234","2"]] -> [["1234","1234"],["1234","2"],["1234","1234"],["1234","2"]]
-- collapse [["12","34"]]] -> [["13"],["14"],["23"],["24"]]


-- helper functions ---------------------------------------


-- box side of a board
--split :: Int -> [a] -> [[a]]
split :: Int -> Line a -> Grid a
split _ [] = []
split n xs =  take n xs : split n (drop n xs)

-- 
--splitBox :: Int -> [[a]] -> [[[a]]]
splitBox :: Int -> Grid a -> [Grid a]
splitBox _ [] = []
splitBox n (x:xs) = [split n x] ++ splitBox n xs

--validGrid :: Game -> Bool
validGrid [] = []
validGrid (x:xs) = validLine x : validGrid xs

-- verify distinct elements in line
--validLine :: Line a -> Bool
validLine xs = validLineElems xs && validLineRange xs && validLineLength xs

-- verify distinct elements (not very good On^2)
--validLineElems :: Line a -> Bool
validLineElems xs = grid == nub grid
    where grid = lineElements xs

-- verify line elements
--validLineRange :: Line a -> Bool
validLineRange xs = and [elem x [1..llen] | x <- grid]
    where grid = lineElements xs

-- verify line length
validLineLength :: Line a -> Bool
validLineLength xs = (length xs == llen)

-- list of valid elements in a lin/col/box (with removed dots)
lineElements :: [Char] -> [Int]
lineElements xs = map (\y -> read [y]::Int)  [x | x <- xs, x /= '.']


-- not used (downloaded function)
str2lst (x:xs) = map mylst x : str2lst xs
    where mylst = (\y -> read [y]::Int)

-- not used (downloaded function)
--transp:: [[a]]->[[a]]
transp :: Grid a -> Grid a
transp ([]:_) = []
transp x = (map head x) : transp (map tail x)
-- map head [[1,2,3],[4,5,6],[7,8,9]] -> [1,4,7]
-- map tail [[1,2,3],[4,5,6],[7,8,9]] -> [[2,3],[5,6],[8,9]]



main :: IO ()
main = do
    starttime <- getClockTime
    putStrLn ("> Sudoku Game")
    putStrLn ("game: "++ show game)
    putStrLn ("lin': "++ show (lin' game))
    putStrLn ("col': "++ show (col' game))
    putStrLn ("box': "++ show (box' game))
    putStrLn ("length: "++ show llen ++ ", box side: "++ show bside ++ ", sum vals: "++ show lsize )
    putStrLn ("")
    putStrLn ("> validating..")
    putStrLn ("lin': "++ show (validGrid . lin' $ game)++" -> "++ show (and.validGrid . lin' $ game))
    putStrLn ("col': "++ show (validGrid . col' $ game)++" -> "++ show (and.validGrid . col' $ game))
    putStrLn ("box': "++ show (validGrid . box' $ game)++" -> "++ show (and.validGrid . box' $ game))
    putStrLn ("game validates: "++ show (valid' game))
    putStrLn ("")
    putStrLn ("> solving..")
    print (solve3 game) 
    endtime <- getClockTime
    let diff = diffClockTimes endtime starttime
    putStrLn("run time: "++ show (timeDiffToString diff))
   
 
