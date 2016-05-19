import Data.List
import Data.Char
main:: IO()

main = do 
	body [["/"]] [	[["Group1"],["1000"],["root"],["secundario"]]	]
--	body [["/"]] [[[""],[""],[""],[""]]]
	
	
{-	 
	This is the main loop, where the input line is taken, according to the correct command
-}		
body xe userGroupList = do 

	putStr "$"
	op <- getLine


	{-	Command : #groupadd <nombre del grupo> 
		Here is possible to add a new user group
	-}	
	if  head(words(op)) == "groupadd" && length(words(op))==2 then do			--The sinstaxis must be correct
		let id = (read ((((last(userGroupList))!!1)!!0)) :: Integer)+1 			--Increase the counter
		addUserGroup xe userGroupList [[(words(op)!!1)],[ show (id) ],[],[	]]	--Call the function to add the new group
	
	{- 	Command : # useradd -g primaryGroup [-G secondaryGroup1 secondaryGroup2] userName
		An user must have an primary group associated, the secondary ones are optional
	-}
	else if head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))>=4 then do
		--[GPrimario, Gsecundario, nombreUsuario ]
		--agregar a los grupos
		--crear el path correcto
				
	else if head(words(op)) == "pvcreate" then do
		line <- getLine
		addFiles xe [line] userGroupList	
	
	else if head(words(op)) == "vgcreate" then do
		line <- getLine
		findFiles xe line userGroupList
	
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
	body xe userGroupList
	
	
	
	
{-
	To add the user group with all its information
-}
addUserGroup xe userGroupList add = do
	if  then do
		let tmp = userGroupList ++ [add]
		putStrLn $ show tmp
		body xe tmp   
	else do
		putStrLn "The User Group aready exits"

findFiles xe find userGroupList = do
	if [find] `elem` xe then do
		putStrLn "Si existe"
		
		
----------------------------------------------------------------
--addFiles create a new directory in the actual 
addFiles xe add userGroupList = do
	putStrLn  "hola"
	let tmp = xe ++ [add]
	putStrLn $ show tmp
	body tmp userGroupList   

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
	    

