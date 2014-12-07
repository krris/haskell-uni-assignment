import System.IO  
import Data.Array
import qualified Data.List as List


-- input data - it will be read from file (TODO)
inputRows = [1, 0, 2, 1, 2, 1]
inputColumns = [1, 1, 2, 1, 1, 1]  
houses :: [(Int, Int)]
houses = [(0, 1), (3, 2), (3, 4), (4, 0), (4, 4), (5, 2), (5, 5)]

gasPlacement = [(0, 2), (2, 2), (2, 4), (3, 0), (4, 3), (4, 5), (5, 1)]

house = "H"
gas = "g"
empty = "."
delimiter = "|"

-- initialize a board with empty values
emptyBoard :: Array (Int,Int) String
emptyBoard = array ((0,0), (length inputColumns, length inputRows)) 
        [ ((c, r), empty) | c <- [0..length inputColumns], r <- [0..length inputRows]]

-- place houses on a board
board = emptyBoard Data.Array.// [ (id, house) | id <- houses ]

-- place gas on a board
solution = board Data.Array.// [ (id, gas) | id <- gasPlacement ]



-- Pretty print for a board
rows :: Array (Int,Int) a -> [[a]]
rows arr = [[arr ! (r,c) | c <- [clow .. chigh]] | r <- [rlow .. rhigh]]
  where
    ((rlow,clow),(rhigh,chigh)) = bounds arr

rowStrings :: Array (Int,Int) String -> [String]
rowStrings arr = [unwords (List.intersperse "|" row) | row <- rows arr]

tableString :: Array (Int,Int) String -> String
tableString arr = unlines (rowStrings arr)

prettyPrint :: Array (Int,Int) String -> IO ()
prettyPrint arr = putStr (tableString arr)


main = do putStrLn "Input data:"
          prettyPrint board
          putStrLn "Solution:" 
          prettyPrint solution