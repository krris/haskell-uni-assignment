import System.IO 

main = do 
	cont <- readFile "input-file.txt"
	putStrLn cont
	putStrLn ""
	print $ lines cont
	
--gg :: String -> [String]
--gg x = lines x