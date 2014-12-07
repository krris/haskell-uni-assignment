import System.IO  
import Data.Array
import qualified Data.List as List


-- input data - it will be read from file (TODO)
inputRows = [1, 0, 2, 1, 2, 1]
columns = [1, 1, 2, 1, 1, 1]  
inputData = [(0, 1), (3, 2), (3, 4), (4, 0), (4, 4), (5, 2), (5, 5)]

house = "H"
gas = "g"
empty = "."
delimiter = "|"

-- initialize a board with empty values
board :: Array (Int,Int) String
board = array ((0,0), (length columns, length inputRows)) [ ((c, r), empty) | c <- [0..length columns], r <- [0..length inputRows]]

rows :: Array (Int,Int) a -> [[a]]
rows arr = [[arr ! (r,c) | c <- [clow .. chigh]] | r <- [rlow .. rhigh]]
  where
    ((rlow,clow),(rhigh,chigh)) = bounds arr

--rowStrings :: Show a => Array (Int,Int) a -> [String]
rowStrings arr = [unwords (List.intersperse "|" row) | row <- rows arr]

--tableString :: Show a => Array (Int,Int) a -> String
tableString arr = unlines (rowStrings arr)

--prettyPrint :: Show a => Array (Int,Int) a -> IO ()
prettyPrint arr = putStr (tableString arr)

