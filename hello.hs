import Data.List
import Data.Char
main:: IO()

main = do 
	putStrLn "Inicio del File System"
	body ["/","/"]

body xe = do 
	putStrLn "Las carpetas actuales son"
	putStrLn $ show xe
	putStrLn "Digite in para ingresar o find para buscar"
	op <- getLine
	
	if op == "in" then do
		putStrLn "ingrese el nombre de la carpeta"
		line <- getLine
		addFiles xe line
		
	else if op == "ls" then do
		listFiles 0 xe
		--line <- getLine
		--findFiles xe line
		
	else if op == "find" then do
		putStrLn "Digite el nombre de la carpeta a buscar"
		--line <- getLine
		--findFiles xe line
	
	else do
		putStrLn $ "digite el comando correcto"
		body xe		
----------------------------------------------------------------
--addFiles create a new directory in the actual 	
addFiles xe add  = do
	let tmp = xe ++ [head xe ++ add]
	putStrLn $ show tmp
	body tmp 
----------------------------------------------------------------	  
--called with ls command show all the  directories in the actual
listFiles cont xe = do
	if (cont < length xe) then do

		if (head xe `isInfixOf` (xe !! cont)) && (head xe /= (xe !! cont))  then do
			putStrLn $ xe !! cont
			let tmp = addOne cont
			listFiles tmp xe
	
	
		else do 
			let tmp = addOne cont
			listFiles tmp xe
	else do
		body xe
---------------------------------------------------------------
--move to a directory in the actual
moveDirectory dir xe = do
	if dir `elem` xe then do
		let tmp = dir : tail xe
		body tmp
	else do
		putStrLn "No existe"
	
---------------------------------------------------------------
addOne x = x + 1
	
	

--findFiles xe find  = do
--	if find `elem` xe then do
--		putStrLn "Si existe"
--	else do
--		putStrLn "no existe"
		
--	body xe
	
	    

