import System.IO  
import qualified Data.Map as Map

-- input data - it will be read from file (TODO)
rows = [1, 0, 2, 1, 2, 1]
columns = [1, 1, 2, 1, 1, 1]  
inputData = [(0, 1), (3, 2), (3, 4), (4, 0), (4, 4), (5, 2), (5, 5)]


-- create tuples ((Int, Int), Bool) which describe an element in a table (coordinates and information if an element has value or not)
emptyElements = Map.fromList [ ((x, y), False) | x <- [0..length columns], y <- [0..length rows]]
houses = Map.fromList [ ((x, y), True) | (x, y) <- inputData]

-- change boolean values to some kind of description: H - house, . - empty place
allElements = Map.union houses emptyElements
f = \x -> if x then "|H|" else "|.|"
allElementsWithDescription = Map.map f allElements 

-- pretty print for table with houses
prettyPrintBis map (r, c) | c == (length columns - 1) && r == (length rows - 1)   = map Map.! (r, c) 
                       | c == (length columns - 1)                                 = map Map.! (r, c) ++ "\n" ++ prettyPrintBis map (r + 1, 0)
                       | otherwise = map Map.! (r, c) ++ prettyPrintBis map (r, c + 1)


prettyPrint map = prettyPrintBis map (0, 0)

main = do  putStrLn (prettyPrint allElementsWithDescription)