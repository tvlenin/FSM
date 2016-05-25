import Data.List
import Data.Char
main:: IO()

main = do 
--	body [["/"]] [	[["Group1"],["1000"],["root"],["secundario"]]	] 
--	body [["/"]] [[[""],[""],[""],[""]]]
	--body [	[ 	["groot"],["1000"],["root","roo2"],[] 	]	]	
	body [[["groot"],["1000"],["root" ],[] ]] [["root","1000"] ]
	
{-	 
	This is the main loop, where the input line is taken, according to the correct command-}		
body userGroupList userID = do 

	putStr "$"
	op <- getLine


	{-	Command : #groupadd <nombre del grupo> 
		Here is possible to add a new user group-}	
	if  head(words(op)) == "groupadd" && length(words(op))==2 then			--The sinstaxis must be correct
		addUserGroup userID userGroupList [[(words(op)!!1)],[ show ((read ((((last(userGroupList))!!1)!!0)) :: Integer)+1) ],[],[]]	--Call the function to add the new group
	
	else if (head(words(op)) == "show") && (head(tail(words(op))) == "groups") && length(words(op))==2 then do
		putStrLn $ "GroupName\t\tGID\t\tAssociated Primary Users \t\t AssociatedAsSecondaryUsers"
		showAllgroups userID userGroupList 0
	
	else if (head(words(op)) == "show") && (head(tail(words(op))) == "users") && length(words(op))==2 then do
		putStrLn $ "UserName \t\t UID \t\t PrimaryGroup \t\t SecondaryGroups \t\t HomeDirectory"
		showAllUsers userID [] userGroupList [] 0
		
		
	{- 	Command : # useradd -g primaryGroup [-G secondaryGroup1 secondaryGroup2] userName
		An user must have an primary group associated, the secondary ones are optional-}
	else if head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))==4 || ( head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G" ) then
		createNewUser userID userGroupList userGroupList (init(tail(tail(words(op))))) (last(tail(tail(words(op)))))	0
	
	{- 	Command : # finger <username>
	Displays information about the specified user 
	-}
	else if  head(words(op)) == "finger" && length(words(op))==2 then			--The sinstaxis must be correct
		findUser userID userGroupList ((words(op))!!1) 0



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
		(body userGroupList userID)
	
	
	
	
{-
	To add the user group with all its information-}
addUserGroup userID userGroupList add = do
	checkAndAdd userID userGroupList 0 add 
	
checkAndAdd userID userGroupList i add = do
	if( i<= (length(userGroupList)-1)) then do
		if ( (((userGroupList!!i)!!0)!!0) == ((add!!0)!!0)) then do		--this means the user group already exits
			putStrLn $ "The user group already exits"
			body userGroupList userID 
		else do
			checkAndAdd userID userGroupList (i+1) add					--check the name of the next user group			
	else do											--The user group does not exits
		putStrLn $ (show userGroupList)
		putStrLn $ ("agredando!: "++show(add))
		body (userGroupList++[add]) userID


{-Here is the display of GroupName -> GroupId -> AssociatedUsers -> AssociatedSecondaryUsers
-}
showAllgroups userID userGroupList i = do
	if( i<= (length(userGroupList)-1) ) then do				--there are still groups to print			
		--putStrLn $ (show ( (((userGroupList!!i)!!0)!!0) ++ "\t" ++ (((userGroupList!!i)!!1)!!0) ++"\t" ++((userGroupList!!i)!!2) ++ "\t"++ ((userGroupList!!i)!!3) ) )
		putStrLn $ (( ((((userGroupList!!i)!!0)!!0)) ++ "\t\t\t" ++ (((userGroupList!!i)!!1)!!0) ++"\t\t\t" ++(show((userGroupList!!i)!!2)) ++ "\t\t\t" ++ (show((userGroupList!!i)!!3)) ) )		
		showAllgroups userID userGroupList (i+1)
	else do
		body userGroupList userID

{-This prints all the required information about all the users.
-}
showAllUsers userID userGroupList toCheck information j = do
	-- if ( null toCheck ) then do
	-- 	printInformation userGroupList information 0
	-- else do
	-- 	if ( (length((toCheck!!0))) ) then do --Just if the j number is still inside the bounds
			
	-- 	else do 
		putStrLn"imprimiendo"

{-
createNewUser <List Of Users> <args> <name>-}
createNewUser userID userGroupList toCheck args name j = do
	if ( (length(toCheck) == 0)) then do 	
		if (null args ) then do
			addNewUserPrimary userID [] userGroupList 0 (head(args)) [] name
		else do
			if(length(args)==1) then do
				addNewUserPrimary userID [] userGroupList 0 (head(args)) [] name
			else do
				addNewUserPrimary userID [] userGroupList 0 (head(args)) (tail(tail(args))) name
	else do
		if ( ((length(((toCheck!!0)!!2)))-1) >= j ) then do					--To check if we check all the names in a user group 
			if ( (((toCheck!!0)!!2)!!j) /= name ) then do						--j+1 iterates over the same user groups to check if exits 
				createNewUser userID userGroupList toCheck args name (j+1)	
			else do																--We found a user with the same name
				putStrLn "userAdd -> Error el usuario existe"
				body userGroupList userID
		else do																--Start checking another user group
			createNewUser userID userGroupList (tail(toCheck)) args name 0

addNewUserPrimary userID userGroupList toCheck j primary secondary name = do
	if ((length(toCheck))==0) then do 							--We checked the whole group names but failed
		putStrLn "userAdd -> The primary group does not exits"
		body userGroupList userID
	else do
		if ( (((toCheck!!0)!!0)!!0) ==  primary ) then do	--This means we found the correct group to add the user as primary 			
			addNewUserSecondary (userID++[[name,  show((read((last(userID))!!1)::Integer)+1)  ]]) [] (userGroupList ++ [ [((toCheck!!0)!!0)] ++ [((toCheck!!0)!!1)] ++ [(((toCheck!!0)!!2)++[name])] ++ [((toCheck!!0)!!3)] ] ++ (tail(toCheck)) ) 0 (secondary) name
		else do
			--2 casos:
			--a. j ya se va a salir del grupo, significa que paso de grupo y lo agrego a userGroupList
			--b. aumentar j para revisar otro usuario dentro del mismo userGroupList
			if( ( (length (((toCheck!!0)!!2))) -1 ) >= j ) then do
				addNewUserPrimary userID userGroupList toCheck (j+1) primary secondary name -- To compare the next one, inside the same group
			else do
				if(length(toCheck)==1) then do
					addNewUserPrimary userID (userGroupList++[(head(toCheck))]) [] 0 primary secondary name
				else do 
					addNewUserPrimary userID (userGroupList++[(head(toCheck))]) (tail(toCheck)) 0 primary secondary name
			
addNewUserSecondary userID userGroupList toCheck j secondary name = do
	if( null secondary) then do
		body (userGroupList++toCheck) userID
	else if ( null toCheck ) then do
		putStrLn "The secondary User Group does not exits"
		body (userGroupList++toCheck) userID
	else do
		if ( (((toCheck!!0)!!0)!!0) == (head(secondary)) ) then do --This user will be added as a secondary to this user group
			addNewUserSecondary userID (userGroupList ++ [ [(toCheck!!0)!!0]++ [(toCheck!!0)!!1] ++ [(toCheck!!0)!!2] ++[ ((toCheck!!0)!!3)++[name] ] ] ++ tail(toCheck)) (tail(toCheck)) 0 (tail(secondary)) name
		else do
			addNewUserSecondary userID (userGroupList ++ [head(toCheck)]) (tail(toCheck)) 0 secondary name 
	
	
findUser userID userGroupList name j = do
	if ( j<=(length(userID)-1)) then do 
		if(((userID!!j)!!0)==name) then do
			printUserInformation [] userID name ((userID!!j)!!0) userGroupList 0 0 [] []
		else do
			findUser userID userGroupList name (j+1)
			
	else do
		putStrLn $ "User not found"
		body userGroupList userID
--Username
--UID		path
--Associated primary group:
--Associated secondary groups:
printUserInformation userGroupList userID name id toCheck i j asPrimary asSecondary= do
	if(null toCheck) then do
		if (null asSecondary)then do
			putStrLn $ ("Username: "++name++" \n UID:"++id++"\t HomeDirectory:"++"/home/"++name++"\n\n User Primary Group: "++(show(asPrimary)))
			body userGroupList userID
		else do
			putStrLn $ ("Username: "++name++" \n UID:"++id++"\t HomeDirectory:"++"/home/"++name++"\n\n User Primary Group: "++(show(asPrimary))++"\n\n User Secondary Groups:"++(show(asSecondary)))
			body userGroupList userID			
	else if ( i <= ( (length((toCheck!!0)!!2)))-1 ) then do		--There are still users as primary in the list of primary, otherwise we move to the users set as secondary
		if ( (((toCheck!!0)!!2)!!i) == name ) then do 								--The user is as Primary in this User Group
			printUserInformation userGroupList userID name id toCheck (i+1) j (((toCheck!!0)!!2)!!i) asSecondary
		else do																		--Let's check the next user set as primary
			printUserInformation userGroupList userID name id toCheck (i+1) j asPrimary asSecondary
	else if ( j <= (length((toCheck!!0)!!3))-1 ) then do
		if ( (((toCheck!!0)!!3)!!j)==name ) then do 								--The user is as secundary in this User Group
			printUserInformation userGroupList userID name id toCheck i (j+1) asPrimary (asSecondary++[(((toCheck!!0)!!3)!!j)])
		else do																		--Let's check the next user set as secundary
			printUserInformation userGroupList userID name id toCheck i (j+1) asPrimary asSecondary
	else do	--This case means we go to the next group
		printUserInformation (userGroupList++ [head(toCheck)]) userID name id (tail(toCheck)) 0 0 asPrimary asSecondary
				
	

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
	    

