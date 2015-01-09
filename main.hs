import System.IO  
import Data.Array 
import Data.Maybe (fromJust)
import Data.Char 
import Data.List.Split
import Utils
import Validation
import qualified Data.List as List


 --input data - it will be read from file (TODO)
--inputRows :: [Int]
--inputRows = [1, 0, 2, 1, 2, 1]
--inputColumns :: [Int]
--inputColumns = [1, 1, 2, 1, 1, 1]  
--houses :: [(Int, Int)]
--houses = [(0,4),(1,0),(2,3),(2,5),(4,3),(4,4),(5,5)]

inputRows :: [Int]
inputRows = [1, 2, 0, 3, 0, 3]
inputColumns :: [Int]
inputColumns = [2, 1, 1, 2, 1, 2]  
houses :: [(Int, Int)]
houses = [(0,4),(1,1),(1,3),(2,5),(3,4),(4,0),(4,4),(4,5),(5,2)]

--inputRows :: [Int]
--inputRows = [5, 1, 3, 2, 5, 1, 3, 1, 4, 3]
--inputColumns :: [Int]
--inputColumns = [4, 0, 3, 1, 2, 1, 3, 2, 0, 5, 0, 3, 1, 3]  
--houses :: [(Int, Int)]
--houses = [
--          (0,0), (0,8),
--          (1,0), (1,3), (1,4), (1,5),
--          (2,7),
--          (3,5),
--          (4,9),
--          (5,0),(5,3),
--          (6,3),(6,7),(6,8),
--          (7,0),
--          (8,2),(8,4),
--          (9,1),(9,5),(9,9),
--          (10,4),
--          (11,5),(11,7),
--          (12,1),(12,2),
--          (13,2),(13,5),(13,8)
--          ]

--inputRows :: [Int]
--inputRows = [5, 1, 4, 1, 4, 2, 4, 2, 1, 4]
--inputColumns :: [Int]
--inputColumns = [3,2,0,5,0,5,0,2,2,3,0,2,2,2]  
--houses :: [(Int, Int)]
--houses = [
--          (0,1), (0,5), (0, 6),
--          (1,3), (1,9),
--          (2,5), (2,9),

--          (4,0), (4,2), (4,3), (4,4), (4,7),
--          (5,5),
--          (6,0),(6,9),
--          (7,0), (7,6),
--          (8,2),(8,5),(8,8),
--          (9,5),
--          (10,0),
--          (11,2),
--          (12,5),(12,6), (12,8),
--          (13,0)
--          ]

gasPlacement :: [(Int, Int)]
gasPlacement = [(0,3),(1,5),(2,0),(2,2),(3,4),(4,2),(5,4)]
wrongPlacement = [(0,5),(1,3),(2,0),(2,2),(3,4),(4,2),(5,4)]

-- House and gas descriptors used for displaying result
houseDesc   = "H"
gasDesc     = "+"
emptyDesc   = " "
delimiter   = "|"

columnLength = length inputColumns - 1 --(length (getColumn dat) - 1)
rowsLength   = length inputRows - 1 --(length (getRow dat) - 1)

--houses  = (getHouses dat)
--inputColumns = (getColumn dat)
--inputRows = (getRow dat)



-- Generates all possible coordinates where gas can be placed
gasPossiblePlacement :: String -> [(Int, Int)]
gasPossiblePlacement dat = removeDups (deleteAll (getHouses dat)
                       [ (x + 1, y) | (x,y) <- (getHouses dat), x < (length (getColumn dat) - 1)] ++
                       [ (x - 1, y) | (x,y) <- (getHouses dat), x > 0] ++
                       [ (x, y + 1) | (x,y) <- (getHouses dat), y < (length (getRow dat) - 1)] ++
                       [ (x, y - 1) | (x,y) <- (getHouses dat), y > 0])


-- Get all indices of elements which has value equal to 0 from a list  
getZeroIndices  xs          = getZeroIndices' 0 xs
getZeroIndices' _   []      = []
getZeroIndices' n   (x:xs)  = if x == 0 then [n] ++ getZeroIndices' (n+1) xs
                              else getZeroIndices' (n+1) xs

-- Filter out every gas which is placed in column/row labeled with 0
filterOutRowsAndColumnsWithZero :: String -> [(Int, Int)]
filterOutRowsAndColumnsWithZero dat = List.filter (\(c, r) -> not (r `elem` getZeroIndices (getRow dat)) && 
                                                          not (c `elem` getZeroIndices (getColumn dat))) $
                                  gasPossiblePlacement dat



-- Checks if given solution (gas placement) has a correct number of gas in every column
hasCorrectColumnLabel :: (Num a, Eq a) => 
            String -> 
			[(a, t)] -> -- gasPlacement
            Bool
hasCorrectColumnLabel dat g = hasCorrectColumnLabel' (getColumn dat) g 0

hasCorrectColumnLabel' :: (Num a, Eq a) => 
              [Int] -> -- list of columns labels
              [(a, t)] -> -- solution (gas placement)
              a -> -- current index of inputColumns
              Bool
hasCorrectColumnLabel' []     g id = True
hasCorrectColumnLabel' (x:xs) g id = 
    if ((List.length gasesAtColumn) <= x) then 
        hasCorrectColumnLabel' xs g (id + 1)
    else False
    where gasesAtColumn = List.filter (\(c, r) -> c == id) g          


-- Checks if given solution (gas placement) has a correct number of gas in every row
hasCorrectRowLabel :: (Num a, Eq a) => String -> [(t, a)] -> Bool
hasCorrectRowLabel dat g = hasCorrectRowLabel' (getRow dat) g 0

hasCorrectRowLabel' :: (Num a, Eq a) => 
              [Int] -> -- list of rows labels
              [(t, a)] -> -- solution (gas placement)
              a -> -- current index of inputRows
              Bool
hasCorrectRowLabel' []     g id = True
hasCorrectRowLabel' (x:xs) g id = if (List.length gasesAtRow <= x) then 
                              hasCorrectRowLabel' xs g (id + 1)
                          else False
    where gasesAtRow = List.filter (\(c, r) -> r == id) g


-- Generates a list of coordinates where gas cannot be placed (every place around gas)
placesNotForGas :: (Num t1, Num t) => [(t, t1)] -> [(t, t1)]
placesNotForGas []      = []
placesNotForGas (x:xs)  = placesNotForGas' x ++ placesNotForGas xs
placesNotForGas' (x, y) = [ 
                            (x+1, y), (x-1, y), 
                            (x, y+1), (x, y-1), 
                            (x+1, y+1), (x+1, y-1), 
                            (x-1, y-1), (x-1, y+1)
                          ]

-- Checks if on the list there are two gas, which are placed next to each other
existTwoGasTouchingEachOther  l           = existTwoGasTouchingEachOther' l l
existTwoGasTouchingEachOther' [x]    _    = False
existTwoGasTouchingEachOther' (x:xs) list = if x `elem` (placesNotForGas list) then True
                                    else existTwoGasTouchingEachOther (xs) 

-- Generates a list of all solutions. One solution is a list of gas coordinates.
findSolutions :: String -> [[(Int, Int)]]
findSolutions dat = filterCombs (\x -> (hasCorrectColumnLabel dat x) == True && 
                                   (hasCorrectRowLabel dat x) == True &&
                                   (existTwoGasTouchingEachOther x) == False)
                  (length (getHouses dat)) $ filterOutRowsAndColumnsWithZero dat

getSolution :: String -> [(Int, Int)]
getSolution dat = getSolution' $ findSolutions dat
getSolution' solutions = if List.length solutions == 1 then solutions !! 0
                        else error "There is more than one solution for given data."




-- *************************************
-- Part of code used for pretty printing
-- *************************************

-- Initialize a board with empty values
emptyBoard :: String -> Array (Int,Int) String
emptyBoard dat = array ((0,0), ((length (getColumn dat) - 1), (length (getRow dat) - 1))) 
        [ ((c, r), emptyDesc) | c <- [0..(length (getColumn dat) - 1)], r <- [0..(length (getRow dat) - 1)]]

-- Place houses on a board
board :: String -> Array (Int, Int) String
board dat = emptyBoard dat Data.Array.// [ (id, houseDesc) | id <- (getHouses dat) ]

-- Prepares a list to be pretty printed
listForPrettyPrint :: String -> [(Int, Int)] -> String -> Array (Int, Int) String
listForPrettyPrint dat list desc = board dat Data.Array.// [ (id, desc) | id <- list ]

-- Pretty print for a board
rows :: Array (Int,Int) a -> [[a]]
rows arr = [[arr ! (c,r) | c <- [clow .. chigh]] | r <- [rlow .. rhigh]]
  where
    ((clow, rlow),(chigh,rhigh)) = bounds arr

-- Add to every row a corresponding number from inputRows
rowsWithId dat arr = appendEveryElem (rows arr) (map show (getRow dat))

rowStrings :: String -> Array (Int,Int) String -> [String]
rowStrings dat arr = [unwords (List.intersperse "|" row) | row <- rowsWithId dat arr]

tableString :: String -> Array (Int,Int) String -> String
tableString dat arr = unlines (rowStrings dat arr)

-- Print inputColumns
printColumnsNumbers :: String -> IO ()
printColumnsNumbers dat = putStrLn (unwords (List.intersperse "|" (map show (getColumn dat))))

prettyPrint' :: String -> Array (Int,Int) String -> IO ()
prettyPrint' dat arr = do 
						printColumnsNumbers dat;
						putStr (tableString dat arr)

prettyPrint :: String -> [(Int, Int)] -> String -> IO ()
prettyPrint dat placement desc = prettyPrint' dat (listForPrettyPrint dat placement desc)

printOnlyHouses :: String -> IO ()
printOnlyHouses dat = prettyPrint dat [] "" 


makeValidation :: String -> String
makeValidation x | mainValidation x = "Input data are correct"
				 | otherwise = "Error of validation input data"

getRow :: String -> [Int]
getRow x = getColumnOrRow((lines x) !! 0)

getColumn :: String -> [Int]
getColumn x = getColumnOrRow((lines x) !! 1)


				 
main = do putStrLn "Put name of input file with data:";
		  nameOfFile <- getLine;
		  cont <- readFile nameOfFile;
		  putStrLn "Start of validation...";
		  putStrLn $ makeValidation cont;
		  putStrLn "Input data:";
          printOnlyHouses cont;
          putStrLn "\nSolution:" 
          prettyPrint cont (getSolution cont) gasDesc 

