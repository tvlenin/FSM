import Data.List
import Data.List.Split
import Data.Char
main:: IO()

main = do 
	putStrLn "Inicio del File System"
	body ["/","/"]

body xe = do 
	--putStrLn "Las carpetas actuales son"
	putStrLn $ show xe
	--putStrLn "Digite in para ingresar o find para buscar"
	op <- getLine
	
	if op == "in" then do
		putStrLn "ingrese el nombre de la carpeta"
		line <- getLine
		addFiles xe line
		
	else if op == "ls" then do
		listFiles 0 xe
		
	else if op == "cd" then do
		line <- getLine
		if line == ".." then do
			moveDirectory ("..") xe
		else do
			moveDirectory (line) xe
		
	else if op == "find" then do
		putStrLn "Digite el nombre de la carpeta a buscar"
			
	else do
		putStrLn $ "digite el comando correcto"
		body xe		
----------------------------------------------------------------
--addFiles create a new directory in the actual 	
addFiles xe add  = do
	if (head xe == "/") then do
		body (xe ++ [head xe ++ add])
	else do
		body (xe ++ [head xe++"/" ++ add])		 
----------------------------------------------------------------	  
--called with ls command show all the  directories in the actual
listFiles cont xe = do
	if (cont < length xe) then do
		if (head xe `isInfixOf` (xe !! cont)) && (head xe /= (xe !! cont))  then do
			putStrLn $ xe !! cont
			listFiles (cont + 1) xe
		else do 
			listFiles (cont +1 ) xe
	else do
		body xe
---------------------------------------------------------------
--move to a directory in the actual
moveDirectory dir xe = do
	if dir == ".." then do 
		if (head xe == "/") then 
			body xe
		else do
			delLast xe	
	else if ((head xe == "/") &&(((head xe)++ dir)) `elem` xe) then do
		body (((head xe)++ dir):tail xe)
	else if ((head xe)++"/"++ dir) `elem` xe then do
		body (((head xe)++"/"++ dir):tail xe)
	else do
		putStrLn "No existe"
		body xe
	
---------------------------------------------------------------
--move to previous directory
delLast xe = do
	if ([last(head xe)] /= "/") then do 
		delLast ((init (head xe)): (tail xe)) 
	else if ([last(head xe)] == head xe) then do 
		body xe 
	else do
		body ((init (head xe)): (tail  xe))
		
	
	

--findFiles xe find  = do
--	if find `elem` xe then do
--		putStrLn "Si existe"
--	else do
--		putStrLn "no existe"
		
--	body xe
	
	    

