import Data.List
import Data.List.Split
import Data.Char
main:: IO()

main = do 
--	body [["/"]] [	[["Group1"],["1000"],["root"],["secundario"]]	] 
--	body [["/"]] [[[""],[""],[""],[""]]]
	--body [	[ 	["groot"],["1000"],["root","roo2"],[] 	]	]	
	body [[["l1"],["1000"],["root" ],[] ], [["l2"],["1001"],[],[]]] [["root","1000"] ]
	
{-	 
	This is the main loop, where the input line is taken, according to the correct command-}		
body userGroupList userID = do 

<<<<<<< HEAD
body xe = do 
	--putStrLn "Las carpetas actuales son"
	putStrLn $ show xe
	--putStrLn "Digite in para ingresar o find para buscar"
=======
	putStr "$"
>>>>>>> a18c7b8ec0e43d485aa20b16868004590ed729a0
	op <- getLine


	if  head(words(op)) == "print" then			--The sinstaxis must be correct
		printuserg userID userGroupList 
		
	{-	Command : #groupadd <nombre del grupo> 
		Here is possible to add a new user group-}	
	else if  head(words(op)) == "groupadd" && length(words(op))==2 then			--The sinstaxis must be correct
		addUserGroup userID userGroupList [[(words(op)!!1)],[ show ((read ((((last(userGroupList))!!1)!!0)) :: Integer)+1) ],[],[]]	--Call the function to add the new group
	
	else if (head(words(op)) == "show") && (head(tail(words(op))) == "groups") && length(words(op))==2 then do
		putStrLn $ "GroupName\t\tGID\t\tAssociated Primary Users \t\t AssociatedAsSecondaryUsers"
		showAllgroups userID userGroupList 0
	
	else if (head(words(op)) == "show") && (head(tail(words(op))) == "users") && length(words(op))==2 then do
		putStrLn $ "UserName \t\t UID \t\t PrimaryGroup \t\t SecondaryGroups \t\t HomeDirectory"
		showAllUsers userID userGroupList userID userGroupList 
		
<<<<<<< HEAD
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
		
=======
		
	{- 	Command : # useradd -g primaryGroup [-G secondaryGroup1 secondaryGroup2] userName
		An user must have an primary group associated, the secondary ones are optional-}
	else if head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))==4 || ( head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G" ) then do
		createNewUser userID [] userGroupList (init(tail(tail(words(op))))) (last(tail(tail(words(op)))))	0
	
	else if ( head(words(op))=="usermod" && (words(op)!!1) =="-g" && length(words(op))==4 || ( head(words(op)) == "usermod" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G"v) then do
		modifyInformation 
		
	{- 	Command : # finger <username>
	Displays information about the specified user 
	-}
	else if  head(words(op)) == "finger" && length(words(op))==2 then do			--The sinstaxis must be correct
		findUser userID userGroupList ((words(op))!!1) 0
		
	else do
		putStrLn $ "Unknown command"
		(body userGroupList userID)
	
	
	
printuserg userID userGroupList = do
	putStrLn $ (show(userGroupList))
	body userGroupList userID
>>>>>>> a18c7b8ec0e43d485aa20b16868004590ed729a0
	
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
--		putStrLn $ (show userGroupList)
--		putStrLn $ ("agredando!: "++show(add))
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
showAllUsers userID userGroupList users groups  = do
	if ( null users ) then do
		body userGroupList userID
	else do
		putStrLn $ ((users!!0)!!0)++"\t\t\t"++((users!!0)!!1)++"\t\t\t"++show(findPrimaryFor ((users!!0)!!0) groups [] 0)++"\t\t\t"++show(findSecundaryFor ((users!!0)!!0) groups [] 0)++"\t\t\t"++" /home/"++((users!!0)!!0)
		showAllUsers userID userGroupList (tail(users)) groups 
		
findPrimaryFor user groups answer n=
	if( n <= (length(groups)-1) ) then do
		if(elem user ((groups!!n)!!2) ) then do
			findPrimaryFor user groups (answer++[((groups!!n)!!0)!!0]) (n+1)
		else do
			findPrimaryFor user groups (answer) (n+1)
	else 
		answer
		
findSecundaryFor user groups answer n=
	if ( n <= (length(groups)-1) ) then do
		if ( elem user ((groups!!n)!!3) ) then do
			findSecundaryFor user groups (answer++[((groups!!n)!!0)!!0]) (n+1)
		else do
			findSecundaryFor user groups (answer) (n+1)
	else 
		answer
		
{-
createNewUser <List Of Users> <args> <name>
	
-}
createNewUser userID userGroupList toCheck args name j = do	
	if ( (length(toCheck) == 0)) then do 							--When this is True, means we can created this user
		if(length(args)==1) then do									--this occurs when there are no secondary groups(-G)
			addNewUserPrimary userID [] userGroupList 0 (head(args)) [] name
		else do														--if there are secondary groups, I extract the -G term
			addNewUserPrimary userID [] userGroupList 0 (head(args)) (tail(tail(args))) name
	else do		
		if (not (elem name ((toCheck!!0)!!2))) then do
			createNewUser userID (userGroupList++[head(toCheck)]) (tail(toCheck)) args name j 
		else do
			putStrLn "The user exists"
			body userGroupList userID

addNewUserPrimary userID userGroupList toCheck j primary secondary name = do	
	if ((length(toCheck))==0) then do 							--We checked the whole group names but failed
		putStrLn "userAdd -> The primary group does not exits"
		body userGroupList userID
	else do
		if ( (((toCheck!!0)!!0)!!0) ==  primary ) then do	--This means we found the correct group to add the user as primary 			
			addNewUserSecondary (userID++[[name,  show((read((last(userID))!!1)::Integer)+1)  ]]) [] (userGroupList ++ [ [((toCheck!!0)!!0)] ++ [((toCheck!!0)!!1)] ++ [(((toCheck!!0)!!2)++[name])] ++ [((toCheck!!0)!!3)] ] ++ (tail(toCheck)) ) 0 0 (secondary) name
		else do
			--2 casos:
			--a. j ya se va a salir del grupo, significa que paso de grupo y lo agrego a userGroupList
			--b. aumentar j para revisar otro usuario dentro del mismo userGroupList
			if( (j <= (length (((toCheck!!0)!!2))) -1 ) ) then do
				addNewUserPrimary userID userGroupList toCheck (j+1) primary secondary name -- To compare the next one, inside the same group
			else do
				if(length(toCheck)==1) then do
					addNewUserPrimary userID (userGroupList++[(head(toCheck))]) [] 0 primary secondary name
				else do 
					addNewUserPrimary userID (userGroupList++[(head(toCheck))]) (tail(toCheck)) 0 primary secondary name
			  		
addNewUserSecondary userID userGroupList toCheck i k secondary name = do
	if( null secondary) then do
		body (userGroupList++toCheck) userID
	else if (null toCheck) then do
		putStrLn $ "A secondary does not exist"++name		
		body (userGroupList++toCheck) userID
	else do
		if( i <= ((length(toCheck))-1) ) then do
			if(k <= (length(secondary))-1) then do
				if ( elem (secondary!!k) (((toCheck!!i)!!0)) ) then do	--We found the right place to add it as secondary											
					concatenate userID userGroupList [] toCheck secondary name 	
				else do
					addNewUserSecondary userID userGroupList toCheck i (k+1) secondary name
			else do
				addNewUserSecondary userID userGroupList toCheck (i+1) (0) secondary name
		else do																						--The secondary does not exist
			(addNewUserSecondary userID (userGroupList++toCheck) [] 0 0 secondary name)
					
concatenate userID userGroupList answer toCheck secondary name =
	if (null toCheck && null secondary) then do
		addNewUserSecondary userID userGroupList (answer) 0 0 [] name
	else if ( null toCheck) then do													--done checking, go back to add as secondary
		addNewUserSecondary userID userGroupList (answer++toCheck) 0 0 secondary name	
	else if(null secondary) then do
		addNewUserSecondary userID userGroupList (answer++toCheck) 0 0 [] name

	else do																		--there are still elements to concatenate
		if( (((toCheck!!0)!!0)!!0) == (secondary!!0) ) then do 							--This is the place to add
			if(length(secondary)<=1 && (length(toCheck))<=1) then do
				concatenate userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) [] [] name
			else if (length(secondary)<=1) then do
				concatenate userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) (tail(toCheck)) [] name 
			else if ((length(toCheck))<=1) then do
				concatenate userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) [] (tail(secondary)) name
			else
				concatenate userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) (tail(toCheck)) (tail(secondary)) name 
		else do
			concatenate userID userGroupList (answer++[head(toCheck)]) (tail(toCheck)) secondary name	
			
{-
-}
modifyInformation =
	putStrLn$ "impriendo"			
	
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
			putStrLn $ ("Username: "++name++" \n UID:"++id++"\t HomeDirectory:"++"/home/"++name++"\n\n User Primary Group: "++(show(findPrimaryFor name userGroupList [] 0)))
			body userGroupList userID
		else do
			putStrLn $ ("Username: "++name++" \n UID:"++id++"\t HomeDirectory:"++"/home/"++name++"\n\n User Primary Group: "++(show(findPrimaryFor name userGroupList [] 0))++"\n\n User Secondary Groups:"++(show(findSecundaryFor name userGroupList [] 0)))
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
				

