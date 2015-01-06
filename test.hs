import System.IO
import Data.Char 
import Data.List.Split

main = do 
	cont <- readFile "input-file.txt"
	putStrLn cont
	putStrLn ""
	print (mainValidation cont)
	print ( pierw cont ) 
	print ( drug cont )
	print ( trzec cont )
	


	
	
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
getHouses :: String -> [(Int,Int)]
getHouses x = makeTuples $ removeComma $ splitOnBrackets (tail $ init x)


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













-------------------------------------------------------------------------------







pierw :: String -> [Int]
pierw x = sprawdzCzyokTab ((lines x) !! 0)

drug :: String -> [Int]
drug x = sprawdzCzyokTab ((lines x) !! 1)

trzec :: String -> [(Int,Int)]
trzec x = makeTuples $ splitSup((lines x) !! 2)


isNawiasOtw :: Char -> Bool 
isNawiasOtw x =  x =='['
isNawiasZam :: Char -> Bool 
isNawiasZam x =  x ==']'
isNawiasOtwZ :: Char -> Bool 
isNawiasOtwZ x =  x =='('
isNawiasZamZ :: Char -> Bool 
isNawiasZamZ x =  x ==')'
isNaw :: String -> Bool
isNaw x = (isNawiasOtw (head x)) && (isNawiasZam (last x))
isPrzec :: Char -> Bool 
isPrzec x =  x ==','
isSpac :: Char -> Bool 
isSpac x =  x ==' '
isZaokrNaw :: Char -> Bool 
isZaokrNaw x =  (x =='(') || (x==')') 

isCzyNawCzyPrzecChar :: Char -> Bool
isCzyNawCzyPrzecChar a = (isDigit a) || (isPrzec a) || (isSpac a) || (isZaokrNaw a)

czyNawCzyPrzecString :: String -> Bool
czyNawCzyPrzecString [] = True
czyNawCzyPrzecString (x:xs) = (isCzyNawCzyPrzecChar x) && (czyNawCzyPrzecString xs)


czyPoprawnaForma :: String -> Bool
czyPoprawnaForma x = (isNaw x) && (czyNawCzyPrzecString (tail(init(x)))) 




przejdzPoWszystkich :: String -> String
przejdzPoWszystkich [] = []
przejdzPoWszystkich (x:xs) | isDigit x = sprLiczby (x:xs)
						   | isNawiasOtwZ x = przejdzPoWszystkich xs
						   | isSpac x = przejdzPoWszystkich xs
						   | isPrzec x = xs
						   | isNawiasZamZ x = []

sprLiczby :: String -> String
sprLiczby [] = []
sprLiczby (x:xs) | not (isDigit x) = []
			     | otherwise = x : (sprLiczby xs)

						   
www :: String -> Bool
www [] = True
www (x:xs) = (isZaokrNaw x) && (sprLiczb xs) 

sprLiczb :: String -> Bool
sprLiczb [] = True
sprLiczb (x:xs) = (isDigit x) && ((isPrzec (head xs)) || (sprLiczb xs))  



splitSup :: String -> [String]
splitSup [] = []
splitSup x | (haveBracket $ rest x) = (spl x) : (splitSup $ rest x)
		   | otherwise = [spl x]

haveBracket :: String -> Bool
haveBracket [] = False
haveBracket (x:xs) | isNawiasZamZ x = True
				   | otherwise = haveBracket xs
		   
rest :: String -> String
rest x | (c/=[]) = tail $ c
	   | otherwise = []
	   where c = dropWhile (/=')') x

spl :: String -> String
spl x = firstBracket $ lastBracket x

firstBracket :: String -> String
firstBracket x = drop (length c + 1) x
				where c = takeWhile (/='(') x				
lastBracket :: String -> String
lastBracket x = takeWhile (/=')') x			
				

czyNumer :: String -> Bool
czyNumer [] = True
czyNumer (x:xs) = (isDigit x)  && (czyNumer xs) 



sprawdzCzyokTab :: String  -> [Int]
sprawdzCzyokTab x | x=="" = error "puste"
			   | not (czyPoprawnaForma x) = error "nie prawidłowe dane"
	           | otherwise  = map toNumber (splitOn "," (tail(init(x))))


toNumber :: String -> Int
toNumber x | x=="" = error "nieprawidlowe dane"
		   | czyNumer (trim x) = read' (trim x)
		   | otherwise = error "nieprawidlowe dane"
