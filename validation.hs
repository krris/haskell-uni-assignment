module Validation where
import Data.Char 
import Data.List.Split

	
-----------Validation--------------	

mainValidation :: String -> Bool
mainValidation x | x == "" = error "Brak danych wejściowych"
				 | not (checkLen $ lines x) = error "Dane zawierają nieprawidłową liczbę linii" 
				 | not (checkColumnOrRow $ ((lines x) !! 0)) = error "Nieprawidłowa forma opisu danych w pierwszej linii pliku"
				 | not (checkColumnOrRow $ ((lines x) !! 1)) = error "Nieprawidłowa forma opisu danych w drugirj linii pliku"
				 | not (checkHouses $ ((lines x) !! 2)) = error "Nieprawidłowa forma opisu danych w trzeciej linii pliku"
				 | otherwise = True
	
----
--check input have 3 lines	
checkLen :: [a] -> Bool
checkLen x = if (length x == 3) then True else False
----

----
--check first or second line of file
checkColumnOrRow :: String -> Bool
checkColumnOrRow x	| not (checkBracket x) = False
					| not (checkComma x) = False
					| otherwise = checkNumbers (splitWithTrim x)


--split string on comma with trim it
splitWithTrim :: String -> [String]
splitWithTrim x = splitOn "," s 
			where s = init (tail x)
	
--check [ and ]	
checkBracket :: String -> Bool
checkBracket [] = False
checkBracket x = ((head x) == '[') && ((last x) == ']')

--check string have ,
checkComma :: String -> Bool
checkComma x = if (length c > 1) then True else False
			where c = splitOn "," x
			
--check is not blank and is number
checkNoWithBlank :: String -> Bool
checkNoWithBlank x = if (x==[]) then False else checkNo	x	
			
--check is number
checkNo :: String -> Bool
checkNo [] = True
checkNo (x:xs) = (isDigit x) && (checkNo xs)

--check number on list
checkNumbers :: [String] -> Bool
checkNumbers [""] = False
checkNumbers ("":_) = False
checkNumbers [] = True
checkNumbers (x:xs) = (checkNoWithBlank $ trim x) && (checkNumbers xs) 

----
			

----			
--check 'houses' line from file
checkHouses :: String -> Bool
checkHouses [] = False
checkHouses x | not (checkBracket x) = False
			  | not (checkCommaOnSplit (tail $ init x) ) = False
			  | not (checkSplitedArray $ splitOnBrackets (tail $ init x) ) = False
			  | otherwise = True
			
--check ( and )	
checkBracket' :: String -> Bool
checkBracket' [] = False
checkBracket' x = ((head x) == '(') && ((last x) == ')')

checkCommaOnSplit :: String -> Bool
checkCommaOnSplit [] = False
checkCommaOnSplit x | (splitOnBrackets x == []) = False
					| (not (checkCommaOnSplitOne $ splitOnBrackets x)) = False
					| otherwise = True

checkCommaOnSplitOne :: [String] -> Bool
checkCommaOnSplitOne [] = True
checkCommaOnSplitOne (x:xs) = (checkComma x) && (checkCommaOnSplitOne xs)

--- "(4,3), (2,1)" -> ["4,3", ", ", "2,1"]
splitOnBrackets :: String -> [String]
splitOnBrackets [] = []
splitOnBrackets x = removeIfEmptyHead $ ([takeWhile (/='(') x] ++ [takeWhile (/=')') (afterOpenBracket x)] ++ splitOnBrackets (afterCloseBracket x))
	
removeIfEmptyHead :: [String] -> [String]
removeIfEmptyHead x | (head x) == [] = (tail x)
					| otherwise = x

afterOpenBracket :: String -> String
afterOpenBracket [] = []
afterOpenBracket x | ((dropWhile (/='(') x) == []) = [] 
			       | otherwise = (tail (dropWhile (/='(') x))

afterCloseBracket :: String -> String
afterCloseBracket [] = []
afterCloseBracket x | ((dropWhile (/=')') x) == []) = [] 
			       | otherwise = (tail (dropWhile (/=')') x))

				   
--- check if string is  number, number
checkTuples :: String -> Bool				   
checkTuples [] = False
checkTuples x = if (length l == 2) then (checkSplited l) else False
		where l = splitOn "," x			   

checkSplited :: [String] -> Bool
checkSplited [] = True
checkSplited (x:xs) = checkNumbers [x] && checkSplited xs


--check string in array [ "number, number", ", ", "number, number" , ... ", ", "number, number"]
checkSplitedArray :: [String] -> Bool
checkSplitedArray [] = True
checkSplitedArray x | (length x == 1) = (checkTuples $ head x)
					| (length x == 2) = False
					| (length x >= 3) = (checkTuples $ head x) && (checkCommaOnly $ head $ tail x) && (checkSplitedArray $ tail $ tail x) 

--check string have , only
checkCommaOnly :: String -> Bool
checkCommaOnly x = if (c == ",") then True else False
			where c = trim x

----

trim :: String -> String
trim x = (unwords (words x))

------------------------------end of validation-------------



-------------------convert strings to array-----------------

----
getColumnOrRow :: String -> [Int]
getColumnOrRow x = map convertToNumber (splitOn "," (tail(init(x))))

convertToNumber :: String -> Int
convertToNumber x = read $ trim x
----

----
getHousesFromString :: String -> [(Int,Int)]
getHousesFromString x = makeTuples $ removeComma $ splitOnBrackets (tail $ init x)


--remove , from ["4,3", ", ", "2,1"] 
--convert ["4,3", ", ", "2,1"] to ["4,3", "2,1"]
removeComma :: [String] -> [String]
removeComma x = filter (\elem -> not(checkCommaOnly elem)) x


--convert ["4,5", "3,2"] to [(4,5), (3,2)]				
makeTuples :: [String] -> [(Int,Int)]
makeTuples x = map makeTuple x				
				
--convert string "number, number" to (number,number)	
makeTuple :: String -> (Int, Int)
makeTuple x = (read $ takeWhile (/=',') x, read $ tail $ dropWhile (/=',') x)
----


---------------------------end of convert------------------------------

			
			
			
			
----------------------------logic validation---------------------------------

logicValidate :: [Int] -> [Int] -> [(Int,Int)] -> Bool
logicValidate row column houses | (not (checkLengthOfHouses row column houses)) = error "Nieprawidłowa liczba domków na planszy"
								| (checkIndexOfRowOrColumn row && checkIndexOfRowOrColumn column) = error "Nieprawidłowy indeks w wierszu lub kolumnie"
								| (not (checkSumOfRowAndColumnEq row column)) = error "Nieprawidłowa liczba elementów w wierszu lub kolumnie"
								| (not (checkIndexOfRow row houses)) = error "Nieprawidłowa wartość określająca położenie domku na planszy"
								| (not (checkIndexOfColumn column houses)) = error "Nieprawidłowa wartość określająca położenie domku na planszy"
								| otherwise = True

--check is any index of row or column greater then length of row or column
checkIndexOfRowOrColumn :: [Int] -> Bool
checkIndexOfRowOrColumn x = if (null (filter (\t -> t<0 || t>=maxIndex) x)) then True else False
		where maxIndex = length x

		
--check sum of index from row is equal to sum of index from column
checkSumOfRowAndColumnEq :: [Int] -> [Int] -> Bool
checkSumOfRowAndColumnEq x y = if (sumHouses x == sumHouses y) then True else False 

--sum houses on row or column
sumHouses :: [Int] -> Int
sumHouses [] = 0
sumHouses (x:xs) = x + sumHouses xs


--check index of row in houses
checkIndexOfRow :: [Int] -> [(Int,Int)] -> Bool
checkIndexOfRow row houses = if (null (filter (\elem -> (fst elem) >= maxInd || (fst elem) < 0) houses)) then True else False
					where maxInd = length row


--check index of column in houses
checkIndexOfColumn :: [Int] -> [(Int,Int)] -> Bool
checkIndexOfColumn column houses = if (null (filter (\elem -> (snd elem) >= maxInd || (snd elem) < 0) houses)) then True else False
					where maxInd = length column


--check is length of houses array less then value from row and column array
checkLengthOfHouses :: [Int] -> [Int] -> [(Int,Int)] -> Bool
checkLengthOfHouses row column houses = if ((length houses) <= maxNumberOfHouses) then True else False
			where maxNumberOfHouses = ((length row)*(length column) `div` 2)


-------------------------------------------------------------------------------