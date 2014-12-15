import System.IO  
import Data.Array 
import Data.Maybe (fromJust)
import qualified Data.List as List
import Debug.Trace

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

--filterCombs (\x -> isTouchingOneFromTheList x) (length houses) filterOutRowsAndColumnsWithZero 
--length $ filterCombs (\x -> (checkColumns x) == True && (checkRows x) == True && (isTouchingOneFromTheList x) == False)  (length houses) filterOutRowsAndColumnsWithZero 

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


gasPlacement :: [(Int, Int)]
gasPlacement = [(0,3),(1,5),(2,0),(2,2),(3,4),(4,2),(5,4)]
gasPlacement2 :: [(Int, Int)]
gasPlacement2 = [(0,3),(2,0),(2,2),(3,4),(4,2),(5,4)]
wrongPlacement = [(0,5),(1,3),(2,0),(2,2),(3,4),(4,2),(5,4)]

-- House and gas descriptors used for displaying result
houseDesc = "H"
gasDesc = "+"
emptyDesc = " "
delimiter = "|"

columnLength = length inputColumns - 1
rowsLength = length inputRows - 1

-- Generates all possible coordinates where gas can be placed
gasPossiblePlacement :: [(Int, Int)]
gasPossiblePlacement = removeDups (deleteAll houses
                       [ (x + 1, y) | (x,y) <- houses, x < columnLength] ++
                       [ (x - 1, y) | (x,y) <- houses, x > 0] ++
                       [ (x, y + 1) | (x,y) <- houses, y < rowsLength] ++
                       [ (x, y - 1) | (x,y) <- houses, y > 0])


-- Get all indices of elements which has value equal to 0 from a list  
getZeroIndices xs = getZeroIndices' 0 xs
getZeroIndices' _ [] = []
getZeroIndices' n (x:xs) = if x == 0 then [n] ++ getZeroIndices' (n+1) xs
                     else getZeroIndices' (n+1) xs

-- Filter out every gas which is placed in column/row labeled with 0
filterOutRowsAndColumnsWithZero = List.filter (\(c, r) -> not (r `elem` getZeroIndices inputRows) && 
                                                          not (c `elem` getZeroIndices inputColumns))
                                  gasPossiblePlacement

-- Generates a list of all possible solutions. One solution is a list of gas coordinates.
gasCombinations :: [[(Int, Int)]]
gasCombinations = filterCombs (\x -> isTouchingOneFromTheList x) (length houses) filterOutRowsAndColumnsWithZero
--gasCombinations = choose filterOutRowsAndColumnsWithZero (length houses)

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
    if ((List.length gasesAtColumn) <= x) then 
        checkColumns' xs g (id + 1)
    else False
    where gasesAtColumn = List.filter (\(c, r) -> c == id) g          

-- Checks if given solution (gas placement) has a correct number of gas in every row
checkRows :: (Num a, Eq a) => [(t, a)] -> Bool
checkRows g = checkRows' inputRows g 0
checkRows' [] g id = True
checkRows' (x:xs) g id = if (List.length gasesAtRow > x) then False
                          else checkRows' xs g (id + 1)
    where gasesAtRow = List.filter (\(c, r) -> r == id) g


-- Generates a list of coordinates where gas cannot be placed.
placesNotForGas :: (Num t1, Num t) => [(t, t1)] -> [(t, t1)]
placesNotForGas [] = []
placesNotForGas (x:xs) = placesNotForGas' x ++ placesNotForGas xs
placesNotForGas' (x, y) = [ 
                        (x+1, y), 
                        (x-1, y), 
                        (x, y+1), 
                        (x, y-1), 
                        (x+1, y+1), 
                        (x+1, y-1), 
                        (x-1, y-1),
                        (x-1, y+1)
                        ]


--testT = [(0,0),(1,3),(2,2),(2,4),(3,5),(4,2),(5,4)]
--testF = [(2, 0), (2, 2), (4, 2), (0, 3), (3, 4), (5, 4), (1, 5)]

-- Checks if on the list there are two elements, which are placed next to each other
isTouchingOneFromTheList l = isTouchingOneFromTheList' l l
isTouchingOneFromTheList' [x] _ = False
isTouchingOneFromTheList' (x:xs) list = if x `elem` (placesNotForGas list) then True
                                    else isTouchingOneFromTheList (xs) 


-- Get a list of solutions. 
findSolutions = filter (\x -> (checkColumns x) == True && (checkRows x) == True && (isTouchingOneFromTheList x) == False) gasCombinations 
-- (\x -> (checkColumns x) == True && (checkRows x) == True && (isTouchingOneFromTheList x) == False )
getSolution = getSolution' findSolutions
getSolution' solutions = if List.length solutions == 1 then solutions !! 0
                        else error "There is more than one solution for given data."




--
-- Part of code used for pretty printing 
--

-- initialize a board with empty values
emptyBoard :: Array (Int,Int) String
emptyBoard = array ((0,0), (columnLength, rowsLength)) 
        [ ((c, r), emptyDesc) | c <- [0..columnLength], r <- [0..rowsLength]]

-- place houses on a board
board :: Array (Int, Int) String
board = emptyBoard Data.Array.// [ (id, houseDesc) | id <- houses ]

-- Prepares a list to be pretty printed
listForPrettyPrint :: [(Int, Int)] -> String -> Array (Int, Int) String
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

prettyPrint' :: Array (Int,Int) String -> IO ()
prettyPrint' arr = do printColumnsNumbers
                      putStr (tableString arr)

prettyPrint :: [(Int, Int)] -> String -> IO ()
prettyPrint placement desc = prettyPrint' (listForPrettyPrint placement desc)
-- Utils

-- Delele all elements which exist in xs from ys
deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll xs ys = filter (\x -> not ( x `elem` xs)) ys

-- Create all possible n-size subsets from given list
choose :: 
      [b] ->  -- source list
      Int -> -- size of subsets
      [[b]] -- list of generated subsets
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

-- Appends to every element of double list one following element from second list
-- Example: 
--      test = [[1,1,1],[2,2,2]]
--      values = [6,7]
--      appendEveryElem test values
--      > [[1,1,1,6],[2,2,2,7]]
--appendEveryElem :: [[a]] -> [a] -> [[a]]
appendEveryElem :: [[a]] -> [a] -> [[a]]
appendEveryElem [] _ = []
appendEveryElem _ [] = []
appendEveryElem (x:xs) (v:vs) = [x ++ [v]] ++ appendEveryElem xs vs

removeDups :: (Ord a) => [a] -> [a]
removeDups = map head . List.group . List.sort

zapWith f    []     ys  = ys
zapWith f    xs     []  = xs
zapWith f (x:xs) (y:ys) = f x y : zapWith f xs ys

filterCombs :: ([a] -> Bool) -> Int -> [a] -> [[a]]
filterCombs p n xs | n > length xs = [] 
filterCombs p n xs = go xs id !! n where
    go    []  ds = [[[]]]
    go (x:xs) ds
        | p (ds' []) = zapWith (++) ([] : map (map (x:)) with) without
        | otherwise  = without
        where
            ds'     = ds . (x:)
            with    = go xs ds'
            without = go xs ds




main = do putStrLn "Input data:"
          prettyPrint houses houseDesc
          putStrLn "\nSolution:" 
          prettyPrint getSolution gasDesc























