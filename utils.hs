module Utils where
import qualified Data.List as List

-- Trim string
trim :: String -> String
trim x = (unwords (words x))


-- Delele all elements which exist in xs from ys
deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll xs ys = filter (\x -> not ( x `elem` xs)) ys

-- Appends to every element of double list one following element from second list
-- Example: 
--      test = [[1,1,1],[2,2,2]]
--      values = [6,7]
--      appendEveryElem test values
--      > [[1,1,1,6],[2,2,2,7]]
appendEveryElem :: [[a]] -> [a] -> [[a]]
appendEveryElem []      _     = []
appendEveryElem _       []    = []
appendEveryElem (x:xs) (v:vs) = [x ++ [v]] ++ appendEveryElem xs vs

-- Remove duplicates from a list
removeDups :: (Ord a) => [a] -> [a]
removeDups = map head . List.group . List.sort

zapWith :: (t -> t -> t) -> [t] -> [t] -> [t]
zapWith f    []     ys  = ys
zapWith f    xs     []  = xs
zapWith f (x:xs) (y:ys) = f x y : zapWith f xs ys

-- Create all possible n-size subsets from given list which satisfy given predicate
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