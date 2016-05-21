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
	
	else if (head(words(op)) == "show") && (head(tail(words(op))) == "groups") && length(words(op))==2 then do
		putStrLn $ "GroupName\t\tGID\t\tAssociatedUserst\tAssociatedAsSecondaryUsers"
		showAllgroups userGroupList userGroupList
	
	{- 	Command : # useradd -g primaryGroup [-G secondaryGroup1 secondaryGroup2] userName
		An user must have an primary group associated, the secondary ones are optional-}
	else if head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))==4 || ( head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G" ) then
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

{-Here is the display of GroupName -> GroupId -> AssociatedUsers -> AssociatedSecondaryUsers
-}
showAllgroups userGroupList tmp = do
	if( ( length(tmp)) == 0) then do
		body userGroupList
	else do
		putStrLn $ ( (((tmp!!0)!!0)!!0) ++"\t\t\t"++(((tmp!!0)!!1)!!0)++"\t\t"++(show(((tmp!!0)!!2)))++"\t\t\t"++(show(((tmp!!0)!!3))))
		showAllgroups userGroupList (tail(tmp))


{-
createNewUser <List Of Users> <args> <name>-}
createNewUser userGroupList toCheck args name j = do
	if ( (length(toCheck) == 0)) then do 	
		if (null args ) then do
			addNewUserPrimary [] userGroupList 0 (head(args)) [] name
		else do
			if(length(args)==1) then do
				addNewUserPrimary [] userGroupList 0 (head(args)) [] name
			else do
				addNewUserPrimary [] userGroupList 0 (head(args)) (tail(tail(args))) name
	else do
		if ( ((length(((toCheck!!0)!!2)))-1) >= j ) then do					--To check if we check all the names in a user group 
			if ( (((toCheck!!0)!!2)!!j) /= name ) then do						--j+1 iterates over the same user groups to check if exits 
				createNewUser userGroupList toCheck args name (j+1)	
			else do																--We found a user with the same name
				putStrLn "userAdd -> Error el usuario existe"
				body userGroupList
		else do																--Start checking another user group
			createNewUser userGroupList (tail(toCheck)) args name 0

addNewUserPrimary userGroupList toCheck j primary secondary name = do
	if ((length(toCheck))==0) then do 							--We checked the whole group names but failed
		putStrLn "userAdd -> The primary group does not exits"
		body userGroupList
	else do
		if ( (((toCheck!!0)!!0)!!0) ==  primary ) then do	--This means we found the correct group to add the user as primary 			
			addNewUserSecondary [] (userGroupList++ [ [((toCheck!!0)!!0)] ++ [((toCheck!!0)!!1)] ++ [(((toCheck!!0)!!2)++[name])] ++ [((toCheck!!0)!!3)] ] ++ (tail(toCheck)) ) 0 (secondary) name
		else do
			--2 casos:
			--a. j ya se va a salir del grupo, significa que paso de grupo y lo agrego a userGroupList
			--b. aumentar j para revisar otro usuario dentro del mismo userGroupList
			if( ( (length (((toCheck!!0)!!2))) -1 ) >= j ) then do
				addNewUserPrimary userGroupList toCheck (j+1) primary secondary name	
				putStrLn "buscar el siguiente dentro del mismo grupo"
			else do
				if(length(toCheck)==1) then do
					addNewUserPrimary (userGroupList++[(head(toCheck))]) [] 0 primary secondary name
				else do 
					addNewUserPrimary (userGroupList++[(head(toCheck))]) (tail(toCheck)) 0 primary secondary name
			
addNewUserSecondary userGroupList toCheck j secondary name = do
	if( null secondary) then do
		body (userGroupList++toCheck)
	else if ( null toCheck ) then do
		putStrLn "The secondary User Group does not exits"
		body (userGroupList++toCheck)
	else do
		if ( (((toCheck!!0)!!0)!!0) == (head(secondary)) ) then do --This user will be added as a secondary to this user group
			addNewUserSecondary (userGroupList ++ [ [(toCheck!!0)!!0]++ [(toCheck!!0)!!1] ++ [(toCheck!!0)!!2] ++[ ((toCheck!!0)!!3)++[name] ] ] ++ tail(toCheck)) (tail(toCheck)) 0 (tail(secondary)) name
		else do
			addNewUserSecondary (userGroupList ++ [head(toCheck)]) (tail(toCheck)) 0 secondary name 
		
	
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
	    

