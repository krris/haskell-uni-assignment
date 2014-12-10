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
houses = [(1, 0), (2, 3), (4, 3), (0, 4), (4, 4), (2, 5), (5, 5)]
--houses = [(0,0), (6,5)]

gasPlacement :: [(Int, Int)]
gasPlacement = [(2, 0), (2, 2), (4, 2), (0, 3), (3, 4), (5, 4), (1, 5)]
gasPlacement2 :: [(Int, Int)]
gasPlacement2 = [(2, 0), (2, 2), (4, 2), (0, 3), (3, 4), (5, 4)]


house = "H"
gas = "+"
empty = " "
delimiter = "|"


columnLength = length inputColumns - 1
rowsLength = length inputRows - 1
-- initialize a board with empty values
emptyBoard :: Array (Int,Int) String
emptyBoard = array ((0,0), (columnLength, rowsLength)) 
        [ ((c, r), empty) | c <- [0..columnLength], r <- [0..rowsLength]]

-- place houses on a board
board = emptyBoard Data.Array.// [ (id, house) | id <- houses ]

-- place gas on a board
solution = board Data.Array.// [ (id, gas) | id <- gasPlacement ]

-- possible gas placement
gasPossiblePlacement = removeDups (deleteAll houses
                       [ (x + 1, y) | (x,y) <- houses, x < columnLength] ++
                       [ (x - 1, y) | (x,y) <- houses, x > 0] ++
                       [ (x, y + 1) | (x,y) <- houses, y < rowsLength] ++
                       [ (x, y - 1) | (x,y) <- houses, y > 0])

-- place gas on a board
gasPossiblePlacementPretty = board Data.Array.// [ (id, gas) | id <- gasPossiblePlacement ]

gasCombinations = choose gasPossiblePlacement (length houses)

-- args: inputColumns, gas placement
checkColumns g = checkColumns' inputColumns g
checkColumns' [] g = trace ("tttt") $ True
checkColumns' (x:xs) g = 
    if ((List.length gasesAtColumn) == x) then 
        trace (" x: " ++ show x ++ "xs: " ++ show xs ++ " len: " ++ show (List.length gasesAtColumn) ++ " gasAtCol: " ++ show gasesAtColumn) 
        $ checkColumns' xs g
    else False
    where gasesAtColumn = trace ("id: " ++ show id) List.filter (\(c, r) -> c == id) g
          id = fromJust (List.elemIndex x inputColumns)  


checkRows g = checkRows' inputRows g
checkRows' [] g = True
checkRows' (x:xs) g = if (List.length gasesAtRow /= x) then False
                          else checkRows' xs g
    where gasesAtRow = List.filter (\(c, r) -> r == id) g
          id =  fromJust (List.elemIndex x inputRows)  
          


findSolution = filter (\x -> (checkColumns x) == True && (checkRows x) == True) gasCombinations







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

choose :: [b] -> Int -> [[b]]
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k


removeDups :: (Ord a) => [a] -> [a]
removeDups = map head . List.group . List.sort

main = do putStrLn "Input data:"
          prettyPrint board
          putStrLn "\nPossible gas placement:"
          prettyPrint gasPossiblePlacementPretty
          putStrLn "\nSolution:" 
          prettyPrint solution























