import System.IO  
import Data.Array
import Data.Maybe
import qualified Data.List as List


-- input data - it will be read from file (TODO)
inputRows :: [Int]
inputRows = [1, 0, 2, 1, 2, 1]
inputColumns :: [Int]
inputColumns = [1, 1, 2, 1, 1, 1]  
houses :: [(Int, Int)]
houses = [(0, 1), (3, 2), (3, 4), (4, 0), (4, 4), (5, 2), (5, 5)]

gasPlacement = [(0, 2), (2, 2), (2, 4), (3, 0), (4, 3), (4, 5), (5, 1)]

house = "H"
gas = "g"
empty = "."
delimiter = "|"


columnLength = length inputColumns - 1
rowsLength = length inputRows - 1
-- initialize a board with empty values
emptyBoard :: Array (Int,Int) String
emptyBoard = array ((0,0), (columnLength, columnLength)) 
        [ ((c, r), empty) | c <- [0..columnLength], r <- [0..rowsLength]]

-- place houses on a board
board = emptyBoard Data.Array.// [ (id, house) | id <- houses ]

-- place gas on a board
solution = board Data.Array.// [ (id, gas) | id <- gasPlacement ]


-- Pretty print for a board
rows :: Array (Int,Int) a -> [[a]]
rows arr = [[arr ! (r,c) | c <- [clow .. chigh]] | r <- [rlow .. rhigh]]
  where
    ((rlow,clow),(rhigh,chigh)) = bounds arr

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

main = do putStrLn "Input data:"
          prettyPrint board
          putStrLn "\nSolution:" 
          prettyPrint solution























