import Data.List
import Data.Char
main:: IO()

main = do 
--	body [["/"]] [	[["Group1"],["1000"],["root"],["secundario"]]	] 
--	body [["/"]] [[[""],[""],[""],[""]]]
	body [	[ 	["groot"],["1000"],["root","roo2"],[] 	]	]
	
{-	 
	This is the main loop, where the input line is taken, according to the correct command-}		
body userGroupList = do 

	putStr "$"
	op <- getLine


	{-	Command : #groupadd <nombre del grupo> 
		Here is possible to add a new user group-}	
	if  head(words(op)) == "groupadd" && length(words(op))==2 then			--The sinstaxis must be correct
		addUserGroup userGroupList [[(words(op)!!1)],[ show ((read ((((last(userGroupList))!!1)!!0)) :: Integer)+1) ],[],[]]	--Call the function to add the new group
	
	{- 	Command : # useradd -g primaryGroup [-G secondaryGroup1 secondaryGroup2] userName
		An user must have an primary group associated, the secondary ones are optional-}
	else if head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))>=4 then
		createNewUser userGroupList userGroupList (init(tail(tail(words(op))))) (last(tail(tail(words(op)))))	0
				
--	else if head(words(op)) == "pvcreate" then do
--		line <- getLine
--		addFiles xe [line] userGroupList	
	
--	else if head(words(op)) == "vgcreate" then do
--		line <- getLine
--		findFiles line userGroupList
	
--	else if op == "ls" then do
--		listFiles 0 xe userGroupList
		--line <- getLine
		--findFiles xe line
	
--	else if op == "find" then do
--		putStrLn "Digite el nombre de la carpeta a buscar"
		--line <- getLine
		--findFiles xe line
		
	else do
		putStrLn $ "Unknown command"
		body userGroupList
	
	
	
	
{-
	To add the user group with all its information-}
addUserGroup userGroupList add = do
	addIfNotExits userGroupList userGroupList add 
addIfNotExits userGroupList toCheck add = do

	if (length(toCheck) == 1) then do
		if ((add!!0)!!0) /= (((toCheck!!0)!!0)!!0) then do
		 	putStrLn "User Group added"
		 	body (userGroupList++[add])
		else do
			putStrLn "User Group already exits"
		 	body (userGroupList)
	else do
		addIfNotExits userGroupList (tail(toCheck)) add	

{-
createNewUser <List Of Users> <args> <name>-}
createNewUser userGroupList toCheck args name j = do
	if ( (length(toCheck) == 0)) then do 
		putStrLn "Se puede crear el usuario"
		addNewUserPrimary userGroupList 0 (head(args)) (tail(args)) name
	else do
		if ( ((length(((toCheck!!0)!!2)))-1) >= j ) then do
			if ( (((toCheck!!0)!!2)!!j) /= name ) then do
				createNewUser userGroupList toCheck args name (j+1)	
			else do
				putStrLn "Error el usuario existe"
				body userGroupList
		else do	
			createNewUser userGroupList (tail(toCheck)) args name 0

addNewUserPrimary userGroupList pos primary secondary name = do
	if ( ( length( userGroupList!!pos ) ) == 0) then do --pos no se debe salir de aqui
		putStrLn "The primary group does not exits"
	else do
		if ( ((toCheck!!0)!!0) ==  primary ) then do
			
		else do
		
		
		

-- createNewUser2 userGroupList args name = do
-- 	if (length(args))==1 then
-- 		verifyNewUser userGroupList userGroupList (head(args)) [] name
-- 	else do
-- 		verifyNewUser userGroupList userGroupList (head(args)) (tail(args)) name 0 0
			
-- verifyNewUser userGroupList toCheck primary secondary name x y =do
-- 	if (length(toCheck))==0 then do
-- 		putStrLn "no existe"
-- 	else do
-- 		if ( ((toCheck!!0)!!2)!!0 == name) then do
-- 			putStrLn "The user already exits"
-- 		else do
-- 			verifyNewUser userGroupList (tail(toCheck)) primary secondary name
					 															

-----------------------------tory in the actual 
{-addFiles xe add userGroupList = do
	putStrLn  "hola"
	putStrLn "User already exits"
main userGroupListtmp = xe ++ [add]
	putStrLn $ show tmp
	body tmp userGroupList   
-}

----------------------------------------------------------------	  
--called with ls command show all the  directories in the actual
{-listFiles cont xe userGroupList = do
	if (cont < length xe) then do

		if (head xe `isInfixOf` (xe !! cont)) && (head xe /= (xe !! cont))  then do
			putStrLn $ (xe!!cont)
			let tmp = addOne cont
			listFiles tmp xe userGroupList	
		else do 
			let tmp = addOne cont
			listFiles tmp xe userGroupList
	else do
		body xe userGroupList	-}		
---------------------------------------------------------------
--move to a directory in the actual
{-moveDirectory dir xe = do
	if dir `elem` xe then do
		let tmp = dir : tail xe
		body tmp
	else do
		putStrLn "No existe"
-}	
---------------------------------------------------------------
--addOne x = x + 1	
	    

