import Data.Array
--import qualified Data.Map as Map

--table :: Array (Int,Int) Int
--table = array ((0,0),(3,5))
--               [((i,j),Map.findWithDefault 0 (i,j) map) | i <- [0 .. 3], j <- [0 .. 5]]
--table :: Array (Int,Int) Int
--table = array (1,3) [ ((1,1), 1), ((1,2), 1), ((1,3), 1)]

table :: Array (Int,Int) Int
table = array ((0,0), (1,2)) [ ((0,0), 1), ((0,1), 2), ((0,2), 3),
                               ((1,0), 4), ((1,1), 5), ((1,2), 6)]


rows :: Array (Int,Int) a -> [[a]]
rows arr = [[arr ! (r,c) | c <- [clow .. chigh]] | r <- [rlow .. rhigh]]
  where
    ((rlow,clow),(rhigh,chigh)) = bounds arr

rowStrings :: Show a => Array (Int,Int) a -> [String]
rowStrings arr = [unwords (map show row) | row <- rows arr]

tableString :: Show a => Array (Int,Int) a -> String
tableString arr = unlines (rowStrings arr)

prettyPrint :: Show a => Array (Int,Int) a -> IO ()
prettyPrint arr = putStr (tableString arr)

