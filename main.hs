import System.IO  
import Data.Array 
--import Data.Maybe
import Data.Maybe (fromJust)
import qualified Data.List as List
import Debug.Trace



-- input data - it will be read from file (TODO)
inputRows :: [Int]
inputRows = [1, 0, 2, 1, 2, 1]
inputColumns :: [Int]
inputColumns = [1, 1, 2, 1, 1, 1]  
houses :: [(Int, Int)]
houses =  List.sort [(1, 0), (2, 3), (4, 3), (0, 4), (4, 4), (2, 5), (5, 5)]
--houses = [(0,0), (6,5)]

gasPlacement :: [(Int, Int)]
gasPlacement = [(0,3),(1,5),(2,0),(2,2),(3,4),(4,2),(5,4)]
gasPlacement2 :: [(Int, Int)]
gasPlacement2 = [(0,3),(2,0),(2,2),(3,4),(4,2),(5,4)]
wrongPlacement = [(0,5),(1,3),(2,0),(2,2),(3,4),(4,2),(5,4)]

-- House and gas descriptors used for displaying result
house = "H"
gas = "+"
empty = " "
delimiter = "|"

columnLength = length inputColumns - 1
rowsLength = length inputRows - 1

-- generates all possible coordinates where gas can be placed
gasPossiblePlacement :: [(Int, Int)]
gasPossiblePlacement = removeDups (deleteAll houses
                       [ (x + 1, y) | (x,y) <- houses, x < columnLength] ++
                       [ (x - 1, y) | (x,y) <- houses, x > 0] ++
                       [ (x, y + 1) | (x,y) <- houses, y < rowsLength] ++
                       [ (x, y - 1) | (x,y) <- houses, y > 0])

-- Generates a list of all possible solutions. One solution is a list of gas coordinates.
gasCombinations :: [[(Int, Int)]]
gasCombinations = choose gasPossiblePlacement (length houses)

-- Checks if given solution (gas placement) has a correct number of gas in every column
checkColumns :: (Num a, Eq a) => 
            [(a, t)] -> -- gasPlacement
            Bool
checkColumns g = checkColumns' inputColumns g 0

checkColumns' :: (Num a, Eq a) => 
              [Int] -> -- list of correct numbers for columns (inputColumns)
              [(a, t)] -> -- solution (gas placement)
              a -> -- current index of inputColumns
              Bool
checkColumns' [] g id = True
checkColumns' (x:xs) g id = 
    if ((List.length gasesAtColumn) == x) then 
        checkColumns' xs g (id + 1)
    else False
    where gasesAtColumn = List.filter (\(c, r) -> c == id) g          

-- Checks if given solution (gas placement) has a correct number of gas in every row
checkRows :: (Num a, Eq a) => [(t, a)] -> Bool
checkRows g = checkRows' inputRows g 0
checkRows' [] g id = True
checkRows' (x:xs) g id = if (List.length gasesAtRow /= x) then False
                          else checkRows' xs g (id + 1)
    where gasesAtRow = List.filter (\(c, r) -> r == id) g


findSolution = filter (\x -> (checkColumns x) == True && (checkRows x) == True && (isTouchingOneFromTheList x) == False ) gasCombinations 

touchingGas [] = []
touchingGas (x:xs) = touchingGas' x ++ touchingGas xs

touchingGas' (x, y) = [ 
                        (x+1, y), 
                        (x-1, y), 
                        (x, y+1), 
                        (x, y-1), 
                        (x+1, y+1), 
                        (x+1, y-1), 
                        (x-1, y-1),
                        (x-1, y+1)
                    ]


testT = [(0,0),(1,3),(2,2),(2,4),(3,5),(4,2),(5,4)]
testF = [(2, 0), (2, 2), (4, 2), (0, 3), (3, 4), (5, 4), (1, 5)]


isTouchingOneFromTheList l = isTouchingOneFromTheList' l l
isTouchingOneFromTheList' [x] _ = False
isTouchingOneFromTheList' (x:xs) list = if x `elem` (touchingGas list) then True
                                    else isTouchingOneFromTheList (xs) 


hasTouchingGas' [] _ = False
hasTouchingGas' (x:xs) t = if x `elem` (touchingGas t) then True
                        else hasTouchingGas' xs t


--
-- Part of code used for pretty printing 
--

-- initialize a board with empty values
emptyBoard :: Array (Int,Int) String
emptyBoard = array ((0,0), (columnLength, rowsLength)) 
        [ ((c, r), empty) | c <- [0..columnLength], r <- [0..rowsLength]]

-- place houses on a board
board = emptyBoard Data.Array.// [ (id, house) | id <- houses ]

-- place gas on a board
solution = board Data.Array.// [ (id, gas) | id <- gasPlacement ]

-- place gas on a board
gasPossiblePlacementPretty = board Data.Array.// [ (id, gas) | id <- gasPossiblePlacement ]

listForPrettyPrint list desc = board Data.Array.// [ (id, desc) | id <- list ]




-- Pretty print for a board
rows :: Array (Int,Int) a -> [[a]]
rows arr = [[arr ! (c,r) | c <- [clow .. chigh]] | r <- [rlow .. rhigh]]
  where
    ((clow, rlow),(chigh,rhigh)) = bounds arr

-- Add to every row a corresponding number from inputRows
rowsWithId arr = appendEveryElem (rows arr) (map show inputRows)

rowStrings :: Array (Int,Int) String -> [String]
rowStrings arr = [unwords (List.intersperse "|" row) | row <- rowsWithId arr]

tableString :: Array (Int,Int) String -> String
tableString arr = unlines (rowStrings arr)

-- Print inputColumns
printColumnsNumbers :: IO ()
printColumnsNumbers = putStrLn (unwords (List.intersperse "|" (map show inputColumns)))

prettyPrint :: Array (Int,Int) String -> IO ()
prettyPrint arr = do printColumnsNumbers
                     putStr (tableString arr)

-- Appends to every element of double list one following element from second list
-- Example: 
--      test = [[1,1,1],[2,2,2]]
--      values = [6,7]
--      appendEveryElem test values
--      > [[1,1,1,6],[2,2,2,7]]
--appendEveryElem :: [[a]] -> [a] -> [[a]]
appendEveryElem [] _ = []
appendEveryElem _ [] = []
appendEveryElem (x:xs) (v:vs) = [x ++ [v]] ++ appendEveryElem xs vs


-- Utils

-- Delele all elements which exist in xs from ys
deleteAll xs ys = filter (\x -> not ( x `elem` xs)) ys

-- Create all possible n-size subsets from given list
choose :: 
      [b] ->  -- source list
      Int -> -- size of subsets
      [[b]] -- list of generated subsets
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k


removeDups :: (Ord a) => [a] -> [a]
removeDups = map head . List.group . List.sort

main = do putStrLn "Input data:"
          prettyPrint board
          putStrLn "wrong solution:"
          prettyPrint (listForPrettyPrint wrongPlacement gas)
          --putStrLn "\nPossible gas placement:"
          --prettyPrint gasPossiblePlacementPretty
          --putStrLn "\nSolution:" 
          --prettyPrint solution























