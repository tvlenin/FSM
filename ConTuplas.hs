{-Cosas que faltan
-Borrar grupos y usuarios
-modificar usuarios
-Obtener el tiempo
-pasar el User al crear
-}







import Data.List
import Data.List.Split
import Data.Char
import Data.Time
--import Data.Either
main:: IO()

-- [(path, size, True),(path, size, True)]

main = do 
	now <- getCurrentTime
	putStrLn "Inicio del File System"
	body [("/","/",getDate now,"15:32","root:root","","d"),("/","/",getDate now,"15:32","root:root","","d")] [("","","")] [[["l1"],["1000"],["root" ],[] ], [["l2"],["1001"],[],[]]] [["root","1000"] ] [] [] [] [] [] []

body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused = do 
	--putStrLn $ show linklist
	putStrLn $ show vglist
	--putStrLn $ show sdlist

	op <- getLine
	if  (op == "") then			--The sinstaxis must be correct
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	
	{-	Command : #groupadd <nombre del grupo> 
		Here is possible to add a new user group-}	
	else if  head(words(op)) == "groupadd" && length(words(op))==2 then			--The sinstaxis must be correct
		--addUserGroup xe xa userID userGroupList [[(words(op)!!1)],[ show ((read ((((last(userGroupList))!!1)!!0)) :: Integer)+1) ],[],[]] sdlist vglist lvlist linklist fslist unused	--Call the function to add the new group
		addUserGroup xe xa userID userGroupList [[(words(op)!!1)],[ show ( (getIntegerFrom ((((last(userGroupList))!!1)!!0)) 0 0) +1) ],[],[]] sdlist vglist lvlist linklist fslist unused	--Call the function to add the new group
 	else if (head(words(op)) == "show") && (head(tail(words(op))) == "groups") && length(words(op))==2 then do
		putStrLn $ "GroupName\t\tGID\t\tAssociated Primary Users \t\t AssociatedAsSecondaryUsers"
		showAllgroups xe xa userID userGroupList 0 sdlist vglist lvlist linklist fslist unused

	else if (head(words(op)) == "show") && (head(tail(words(op))) == "users") && length(words(op))==2 then do
		putStrLn $ "UserName \t\t UID \t\t PrimaryGroup \t\t SecondaryGroups \t\t HomeDirectory"
		showAllUsers xe xa userID userGroupList userID userGroupList sdlist vglist lvlist linklist fslist unused

	{- 	Command : # useradd -g primaryGroup [-G secondaryGroup1 secondaryGroup2] userName
		An user must have an primary group associated, the secondary ones are optional-}
	else if head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))==4 || ( head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G" ) then do
		createNewUser xe xa userID [] userGroupList (init(tail(tail(words(op))))) (last(tail(tail(words(op)))))	0 sdlist vglist lvlist linklist fslist unused

	else if ( head(words(op))=="usermod" && (words(op)!!1) =="-g" && length(words(op))==4) then do 
		modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused (last(words(op))) (words(op)!!2) []
		
	else if( head(words(op)) == "usermod" && (words(op)!!1) =="-G" && length(words(op))>=4 ) then do 
		modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused (last(words(op))) "none" (tail(tail((init(words(op))))))
		
	else if( head(words(op)) == "usermod" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G"  ) then do
		modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused (last(words(op))) ((words(op))!!2) (tail(tail(tail(tail(init(words(op)))))))
		
--	else if ( head(words(op)) == "usermod" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G")) then do
--		modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused (tail(words(op))) ()  
																								-- ^ name 
	{- 	Command : # finger <username>
	Displays information about the specified user 
	-}
	else if  head(words(op)) == "finger" && length(words(op))==2 then do			--The sinstaxis must be correct
		findUser xe xa userID userGroupList ((words(op))!!1) 0 sdlist vglist lvlist linklist fslist unused

	else if  head(words(op)) == "userdel" && length(words(op))==2 then do			--The sinstaxis must be correct
		if( ((words(op))!!1) == "root") then do
			putStrLn$"Well well you are trying to delete root, really?"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		else do 
			deleteUser xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused ((words(op))!!1) 0
		--findUser xe xa userID userGroupList ((words(op))!!1) 0 sdlist vglist lvlist linklist fslist unused
		
	else if  head(words(op)) == "groupdel" && length(words(op))==2 then do			--The sinstaxis must be correct
		deleteGroup xe xa [] userID sdlist vglist lvlist linklist fslist unused ((words(op))!!1) userGroupList
				
	else if ( (head(words(op))=="createdev") && (((words(op))!!1)=="-s") && (length(words(op))==4) && (numberCheck ((words(op))!!2))) then do
		createStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused ((words(op))!!3) ((words(op))!!2) 0
	else if ( (head(words(op))=="createdev") && (((words(op))!!1)=="-s") && (length(words(op))==4) && (not(numberCheck ((words(op))!!2)))) then do
		putStrLn$"Size must be an integer"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		
	else if ( (head(words(op))=="fdisk") && (length(words(op))==2) && (((words(op))!!1)=="-l") ) then do
		--putStrLn$ ""
		listStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused 0
		
	else if ( (head(words(op))=="rmdev") && (length(words(op))==2)) then do
		if( not (haveStorage 0 vglist ((words(op))!!1)) ) then do
			removeStorageDevice xe xa userGroupList userID [] vglist lvlist linklist fslist unused ((words(op))!!1) sdlist
		else 
			putStrLn $"The storage device can't be deleted because it belongs to a volume group"
			
	else if ( (head(words(op))=="pvcreate") && (length(words(op))==2) ) then do
		addLVMtodevice  xe xa userGroupList userID [] vglist lvlist linklist fslist unused ((words(op))!!1) sdlist False
		
	else if (head(words(op)) == "mkdir") &&(head(tail(words(op))) == "-p") && ("/" `isInfixOf` (last(words(op))) ) && (length(words(op))) == 3 then do
		addFiles "d" xe (last(words(op))) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if (head(words(op)) == "mkdir")&& ("/" `isInfixOf` (last(words(op))) ) == False  && (length(words(op))) == 2 then do
		addFiles "d" xe (last(words(op))) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if op == "rm" then do
		putStrLn "ingrese el nombre de la carpeta"
		line <- getLine
		rmFiles xe line xa 	userGroupList userID sdlist vglist lvlist linklist fslist unused
		
		
	else if op == "touch" then do
		putStrLn "ingrese el nombre del archivo"
		line <- getLine
		addFiles "-" xe line xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if op == "echo" then do
		putStrLn "ingrese el archivo a modificar"
		dir <- getLine
		putStrLn "ingrese el contenido del archivo"
		line <- getLine
		echoFile xe xa line dir userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if op == "cat" then do
		putStrLn "ingrese el archivo a ver"
		dir <- getLine
		catFile xe xa dir sdlist vglist lvlist linklist fslist unused
		
	else if op == "ls" then do
		putStrLn "buscando"
		listFiles 0 xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if op == "mv" then do
		putStrLn "Digite la carpte"
		line <- getLine
		moveDirectory line xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		
	else if ( ((length(words(op))) >= 3 ) && (((words(op))!!0) == "vgcreate") ) then do
		
		if ( (alreadyVG 0 (tail(tail(words(op)))) xe) && (listHasLVM 0  (tail(tail(words(op)))) sdlist ) ) then do
			body xe xa userGroupList userID sdlist (addVolumeGroups 0 vglist (words(op)!!1) (tail(tail(words(op)))) (getSumOfStorage sdlist (tail(tail(words(op)))) 0 ) ) lvlist linklist fslist unused		
		else if (not (alreadyVG 0 (tail(tail(words(op)))) xe) ) then do 
			putStrLn "Error creating VG, A volume does not exist"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		-- add the error for a volume that belongs to a VG
		else do
			putStrLn "Error creating VG, a volume has not a LVM"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
				
																		
	else if ( (head(words(op)) == "lvcreate") && (length(words(op)) == 6) && ((words(op)!!1) == "-L") && ((words(op)!!3) == "-n")) then do 
		if((numberCheck ((words(op))!!2))) then do
			valueLogicalVolume xe xa userGroupList userID sdlist [] lvlist linklist fslist unused vglist ((words(op))!!2) ((words(op))!!4) ((words(op))!!5)
		else do 
			putStrLn$"The size must be an integer"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
			
	else if (head(words(op)) == "ln") && (head(tail(words(op))) == "-s") && length(words(op))==4 then do
		if (isNow 0 (head(tail(tail(words(op))))) xe) then do 
			addFiles "-" xe (head(tail(tail(tail(words(op)))))) xa userGroupList userID sdlist vglist lvlist ((linkCreate (head(tail(tail(words(op))))) (head(tail(tail(tail(words(op)))))) "-"):linklist) fslist unused
			--body xe xa userGroupList userID sdlist vglist lvlist ((linkCreate (head(tail(tail(words(op))))) (head(tail(tail(tail(words(op)))))) "d"):linklist) fslist unused 
		else do
			putStrLn "Nop"
	else if (head(words(op)) == "cd") && length(words(op))==2 then do
		if (isLink 0 (head(tail(words(op)))) linklist) && (fi(linklist!!(getLink 0 (head(tail(words(op)))) linklist) ) == "d") then do
			putStrLn "moviendo"
			moveDirectory (s(linklist!!(getLink 0 (head(tail(words(op)))) linklist) )) xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		else do
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if (head(words(op)) == "echo") && length(words(op))==3 then do
		if (isLink 0 (head(tail(words(op)))) linklist) && (fi(linklist!!(getLink 0 (head(tail(words(op)))) linklist) ) == "-") then do
			echoFile xe xa (head(tail(words(op)))) (s(linklist!!(getLink 0 (head(tail(tail(words(op))))) linklist) )) userGroupList userID sdlist vglist lvlist linklist fslist unused
		else do
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		putStrLn $ ("Sintaxis error")
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused

--updateVGstate usersToCheck sdlist = do
	--actualizar el estado de utilziado por un VG a todos los storage device que reciba, ["","","",estado de VG]	
{---------------}
linkCreate pathReal pathLink typePath = do
	(pathLink++"->"++pathReal,pathReal,pathLink,"",typePath,"l","") 

	
getLink cont name linklist = do
	if (cont < length (linklist) )then do
		if t (linklist !!cont ) == name then do 
			cont
		else do
			getLink (cont +1) name linklist 
	else do
		cont
isLink cont name linklist = do
	if (cont < length linklist )then do
		if (t(linklist!!cont) == name)  then do 
			True
		else do
			isLink (cont +1) name linklist 
	else 
		False


{---------------}
			
listHasLVM i usersToCheck sdlist = do
	if(i<=(length(usersToCheck)-1)) then do
		if( isManagedByLVM sdlist (usersToCheck!!i) ) then do
			listHasLVM (i+1) usersToCheck sdlist
		else do
			False						
	else do
		True

{----------------------------------------------------------------Volume Groups----------------------------------------------------------}
alreadyVG cont aList xe = do
	if (cont < length aList) then do
		if (isNow 0 (aList !! cont) xe) == False then do
			False
		else do
			alreadyVG (cont+1) aList xe
	else do
		True

addVolumeGroups cont volumeList vgName listpv size = do
	(volumeList ++ [(vgName,listpv,(length (listpv)), 0,[""],size,size)])
	
	
getSumOfStorage sdlist names answer= do
	if (null sdlist) then do
		answer
	else do
		if( elem (((sdlist)!!0)!!0) names) then do
			getSumOfStorage (tail(sdlist)) names ( answer + (getIntegerFrom ((sdlist!!0)!!1) 0 0) )
		else do 
			if( length(sdlist) == 1) then do
				getSumOfStorage [] names answer
			else do
				getSumOfStorage (tail(sdlist)) names answer
		

	
{-getSumOfStorage sdlist names = do 
	if(null names) then do
		0
	else do
		(sizeByThis sdlist (head(names)) 0) + (getSumOfStorage sdlist (tail(names)) )
			
sizeByThis sdlist name i= do
	if( i <= ((length(sdlist))-1) ) then 
		if( ((sdlist!!i)!!0) == name ) then do 
			((sdlist!!i)!!1)
		else do
			sizeByThis sdlist name (i+1)
	else do	
		0
-}

haveStorage cont vglist sdName = do
	if (cont < (length vglist)) then do
		if (isInVG 0 (s(vglist!!cont)) sdName) then do
			True
		else do
			haveStorage (cont + 1) vglist sdName
	else do
		False
			
		
isInVG cont sdList name = do
	if(cont < (length sdList) ) then do
		if (sdList !! cont) == name then do
			True
		else do
			isInVG (cont + 1) sdList name
	else do
		False
	

{-------------------------------User---Groups----------------------------------------}
printuserg xe xa userID userGroupList sdlist vglist lvlist linklist fslist unused= do
	putStrLn $ (show(userGroupList))
	body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
{-
	To add the user group with all its information-}
addUserGroup xe xa  userID userGroupList add sdlist vglist lvlist linklist fslist unused= do
	checkAndAdd xe xa userID userGroupList 0 add sdlist vglist lvlist linklist fslist unused
	
checkAndAdd xe xa userID userGroupList i add sdlist vglist lvlist linklist fslist unused= do
	if( i<= (length(userGroupList)-1)) then do
		if ( (((userGroupList!!i)!!0)!!0) == ((add!!0)!!0)) then do		--this means the user group already exits
			putStrLn $ "The user group already exits"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		else do
			checkAndAdd xe xa userID userGroupList (i+1) add sdlist vglist lvlist linklist fslist unused					--check the name of the next user group			
	else do											--The user group does not exits
--		putStrLn $ (show userGroupList)
--		putStrLn $ ("agredando!: "++show(add))
		body xe xa (userGroupList++[add]) userID sdlist vglist lvlist linklist fslist unused
		
{-Here is the display of GroupName -> GroupId -> AssociatedUsers -> AssociatedSecondaryUsers
-}
showAllgroups xe xa userID userGroupList i sdlist vglist lvlist linklist fslist unused= do
	if( i<= (length(userGroupList)-1) ) then do				--there are still groups to print			
		--putStrLn $ (show ( (((userGroupList!!i)!!0)!!0) ++ "\t" ++ (((userGroupList!!i)!!1)!!0) ++"\t" ++((userGroupList!!i)!!2) ++ "\t"++ ((userGroupList!!i)!!3) ) )
		putStrLn $ (( ((((userGroupList!!i)!!0)!!0)) ++ "\t\t\t" ++ (((userGroupList!!i)!!1)!!0) ++"\t\t\t" ++(show((userGroupList!!i)!!2)) ++ "\t\t\t" ++ (show((userGroupList!!i)!!3)) ) )		
		showAllgroups xe xa userID userGroupList (i+1) sdlist vglist lvlist linklist fslist unused
	else do
		body xe xa userGroupList userID	sdlist vglist lvlist linklist fslist unused
		
{-This prints all the required information about all the users.
-}
showAllUsers xe xa userID userGroupList users groups sdlist vglist lvlist linklist fslist unused = do
	if ( null users ) then do
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		putStrLn $ ((users!!0)!!0)++"\t\t\t"++((users!!0)!!1)++"\t\t\t"++show(findPrimaryFor ((users!!0)!!0) groups [] 0)++"\t\t\t"++show(findSecundaryFor ((users!!0)!!0) groups [] 0)++"\t\t\t"++" /home/"++((users!!0)!!0)
		if ((length(users))==1) then do
			showAllUsers xe xa userID userGroupList [] groups sdlist vglist lvlist linklist fslist unused
		else do
			showAllUsers xe xa userID userGroupList (tail(users)) groups sdlist vglist lvlist linklist fslist unused 
		
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
createNewUser xe xa userID userGroupList toCheck args name j sdlist vglist lvlist linklist fslist unused= do	
	if ( (length(toCheck) == 0)) then do 							--When this is True, means we can created this user
		if(length(args)==1) then do									--this occurs when there are no secondary groups(-G)
			addNewUserPrimary xe xa userID [] userGroupList 0 (head(args)) [] name sdlist vglist lvlist linklist fslist unused
		else do														--if there are secondary groups, I extract the -G term
			addNewUserPrimary xe xa userID [] userGroupList 0 (head(args)) (tail(tail(args))) name sdlist vglist lvlist linklist fslist unused
	else do		
		if (not (elem name ((toCheck!!0)!!2))) then do
			createNewUser xe xa userID (userGroupList++[head(toCheck)]) (tail(toCheck)) args name j sdlist vglist lvlist linklist fslist unused
		else do
			putStrLn "The user exists"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused

addNewUserPrimary xe xa userID userGroupList toCheck j primary secondary name sdlist vglist lvlist linklist fslist unused = do	
	if ((length(toCheck))==0) then do 							--We checked the whole group names but failed
		putStrLn "userAdd -> The primary group does not exits"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		if ( (((toCheck!!0)!!0)!!0) ==  primary ) then do	--This means we found the correct group to add the user as primary 			
			addNewUserSecondary xe xa (userID++[[name,  show((read((last(userID))!!1)::Integer)+1)  ]]) [] (userGroupList ++ [ [((toCheck!!0)!!0)] ++ [((toCheck!!0)!!1)] ++ [(((toCheck!!0)!!2)++[name])] ++ [((toCheck!!0)!!3)] ] ++ (tail(toCheck)) ) 0 0 (secondary) name sdlist vglist lvlist linklist fslist unused
		else do
			--2 casos:
			--a. j ya se va a salir del grupo, significa que paso de grupo y lo agrego a userGroupList
			--b. aumentar j para revisar otro usuario dentro del mismo userGroupList
			if( (j <= (length (((toCheck!!0)!!2))) -1 ) ) then do
				addNewUserPrimary xe xa userID userGroupList toCheck (j+1) primary secondary name sdlist vglist lvlist linklist fslist unused-- To compare the next one, inside the same group
			else do
				if(length(toCheck)==1) then do
					addNewUserPrimary xe xa userID (userGroupList++[(head(toCheck))]) [] 0 primary secondary name sdlist vglist lvlist linklist fslist unused
				else do 
					addNewUserPrimary xe xa userID (userGroupList++[(head(toCheck))]) (tail(toCheck)) 0 primary secondary name sdlist vglist lvlist linklist fslist unused
			  		
addNewUserSecondary xe xa userID userGroupList toCheck i k secondary name sdlist vglist lvlist linklist fslist unused= do
	if( null secondary) then do
		--body xe xa (userGroupList++toCheck) userID
		addFiles "d" xe ("home/"++name) xa (userGroupList++toCheck) userID sdlist vglist lvlist linklist fslist unused 	
	else if (null toCheck) then do
		putStrLn $ "A secondary does not exist"++name		
		body xe xa (userGroupList++toCheck) userID sdlist vglist lvlist linklist fslist unused
	else do
		if( i <= ((length(toCheck))-1) ) then do
			if(k <= (length(secondary))-1) then do
				if ( elem (secondary!!k) (((toCheck!!i)!!0)) ) then do	--We found the right place to add it as secondary											
					concatenate xe xa userID userGroupList [] toCheck secondary name sdlist	 vglist lvlist linklist fslist unused
				else do
					addNewUserSecondary xe xa userID userGroupList toCheck i (k+1) secondary name sdlist vglist lvlist linklist fslist unused
			else do
				addNewUserSecondary xe xa userID userGroupList toCheck (i+1) (0) secondary name sdlist vglist lvlist linklist fslist unused
		else do																						--The secondary does not exist
			(addNewUserSecondary xe xa userID (userGroupList++toCheck) [] 0 0 secondary name) sdlist vglist lvlist linklist fslist unused

concatenate xe xa userID userGroupList answer toCheck secondary name sdlist vglist lvlist linklist fslist unused=
	if (null toCheck && null secondary) then do
		addNewUserSecondary xe xa userID userGroupList (answer) 0 0 [] name sdlist vglist lvlist linklist fslist unused
	else if ( null toCheck) then do													--done checking, go back to add as secondary
		addNewUserSecondary xe xa userID userGroupList (answer++toCheck) 0 0 secondary name	sdlist vglist lvlist linklist fslist unused
	else if(null secondary) then do
		addNewUserSecondary xe xa userID userGroupList (answer++toCheck) 0 0 [] name sdlist vglist lvlist linklist fslist unused
	else do																		--there are still elements to concatenate
		if( (((toCheck!!0)!!0)!!0) == (secondary!!0) ) then do 							--This is the place to add
			if(length(secondary)<=1 && (length(toCheck))<=1) then do
				concatenate xe xa userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) [] [] name sdlist vglist lvlist linklist fslist unused
			else if (length(secondary)<=1) then do
				concatenate xe xa userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) (tail(toCheck)) [] name sdlist vglist lvlist linklist fslist unused
			else if ((length(toCheck))<=1) then do
				concatenate xe xa userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) [] (tail(secondary)) name sdlist vglist lvlist linklist fslist unused
			else
				concatenate xe xa userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) (tail(toCheck)) (tail(secondary)) name sdlist vglist lvlist linklist fslist unused
		else do
			concatenate xe xa userID userGroupList (answer++[head(toCheck)]) (tail(toCheck)) secondary name	sdlist vglist lvlist linklist fslist unused
	
	
	
	
		
{-
-}
modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name argsPrimary argsSecundary = do
	dellUserModify xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name 0 argsPrimary argsSecundary

dellUserModify xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name i argsPrimary argsSecundary= do
	if (i > ((length(userID)-1))) then do
		putStrLn$"The user does not exist"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do 
		if( name == ((userID!!i)!!0)) then do -- the user exist
			deletePath2 xe ("home/"++((userID!!i)!!0)) xa userGroupList userID sdlist vglist lvlist linklist fslist unused name argsPrimary argsSecundary
			--dellUserModify2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist unused name userID argsPrimary argsSecundary
		else do
			dellUserModify xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name (i+1) argsPrimary argsSecundary
			
dellUserModify2 xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name toCheck argsPrimary argsSecundary = do
	if(null toCheck) then do
		deleteUserFromGroup2 xe xa [] (userID) sdlist vglist lvlist linklist fslist unused userGroupList name argsPrimary argsSecundary-- Erase from the user groups
	else do
		if (name == ((toCheck!!0)!!0) ) then do	
			if( (length(toCheck)) == 1) then do
				dellUserModify2 xe xa userGroupList (userID) sdlist vglist lvlist linklist fslist unused name [] argsPrimary argsSecundary
			else 
				dellUserModify2 xe xa userGroupList (userID) sdlist vglist lvlist linklist fslist unused name (tail(toCheck)) argsPrimary argsSecundary 	
		else 
			if((length(toCheck))==1) then do
				dellUserModify2 xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist unused name [] argsPrimary argsSecundary
			else do
				dellUserModify2 xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist unused name (tail(toCheck)) argsPrimary argsSecundary

deletePath2 xe dir xa userGroupList userID sdlist vglist lvlist linklist fslist unused name argsPrimary argsSecundary= do
	if (isNow 0 dir xe) == False then do
		putStrLn "User deleted, could not delete the path for the user because it does not exist"
		dellUserModify2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist unused name userID argsPrimary argsSecundary
	else if isEmpty xe 0 dir == False then do
		putStrLn "The path for the user still exist, it contain files"
		dellUserModify2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist unused name userID argsPrimary argsSecundary
	else if (isNow 0 dir xe) && isEmpty xe 0 dir then do
		dellUserModify2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist unused name userID argsPrimary argsSecundary
		--body ((take ((getElem 0 dir xe)) xe )++(drop ((getElem 0 dir xe)+1) xe )) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		putStrLn "Error to delete path for the user"
		
deleteUserFromGroup2 xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused toCheck name argsPrimary argsSecundary = do
	if(null toCheck) then do
		if(null argsSecundary) then do
			createNewUser xe xa userID [] userGroupList [argsPrimary] name	0 sdlist vglist lvlist linklist fslist unused
		else do 
			createNewUser xe xa userID [] userGroupList ([argsPrimary,"-G"]++argsSecundary) name	0 sdlist vglist lvlist linklist fslist unused
	else do
		deleteUserFromGroup2 xe xa (userGroupList++[ [ (((toCheck!!0)!!0)),(((toCheck!!0)!!1)),(delName name (((toCheck!!0)!!2))),(delName name (((toCheck!!0)!!3))) ] ]) userID sdlist vglist lvlist linklist fslist unused (tail(toCheck)) name argsPrimary argsSecundary







findUser xe xa userID userGroupList name j sdlist vglist lvlist linklist fslist unused= do
	if ( j<=(length(userID)-1)) then do 
		if(((userID!!j)!!0)==name) then do
			printUserInformation xe xa [] userID name ((userID!!j)!!0) userGroupList 0 0 [] [] sdlist vglist lvlist linklist fslist unused
		else do
			findUser xe xa userID userGroupList name (j+1) sdlist vglist lvlist linklist fslist unused
	else do
		putStrLn $ "User not found"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused





deleteUser xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name i = do
	if (i > ((length(userID)-1))) then do
		putStrLn$"The user does not exist"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do 
		if( name == ((userID!!i)!!0)) then do -- the user exist
			 --deleteUser2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist unused name userID
			deletePath xe ("home/"++((userID!!i)!!0)) xa userGroupList userID sdlist vglist lvlist linklist fslist unused name
		else do
			deleteUser xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name (i+1)

deleteUser2 xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name toCheck= do
	if(null toCheck) then do
		deleteUserFromGroup xe xa [] (userID) sdlist vglist lvlist linklist fslist unused userGroupList name -- Erase from the user groups
	else do
		if (name == ((toCheck!!0)!!0) ) then do	
			if( (length(toCheck)) == 1) then do
				deleteUser2 xe xa userGroupList (userID) sdlist vglist lvlist linklist fslist unused name []
			else 
				deleteUser2 xe xa userGroupList (userID) sdlist vglist lvlist linklist fslist unused name (tail(toCheck)) 	
		else 
			if((length(toCheck))==1) then do
				deleteUser2 xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist unused name []
			else do
				deleteUser2 xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist unused name (tail(toCheck))

deleteUserFromGroup xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused toCheck name= do
	if(null toCheck) then do
		--putStrLn$(show(xe))
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		deleteUserFromGroup xe xa (userGroupList++[ [ (((toCheck!!0)!!0)),(((toCheck!!0)!!1)),(delName name (((toCheck!!0)!!2))),(delName name (((toCheck!!0)!!3))) ] ]) userID sdlist vglist lvlist linklist fslist unused (tail(toCheck)) name
		
delName name toCheck = do
	if(null toCheck) then do
		[]
	else do
		if(name == toCheck!!0) then do
			delName name (tail(toCheck))
		else do 
			[head(toCheck)] ++ (delName name (tail(toCheck)))

deletePath xe dir xa userGroupList userID sdlist vglist lvlist linklist fslist unused name= do
	--putStrLn $ show (isNow 0 dir xe)
	--putStrLn $ show (isEmpty xe 0 dir)
	if (isNow 0 dir xe) == False then do
		putStrLn "User deleted, could not delete the path for the user because it does not exist"
		deleteUser2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist unused name userID
	else if isEmpty xe 0 dir == False then do
		putStrLn "The path for the user still exist, it contain files"
		deleteUser2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist unused name userID
	else if (isNow 0 dir xe) && isEmpty xe 0 dir then do
		deleteUser2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist unused name userID
		--body ((take ((getElem 0 dir xe)) xe )++(drop ((getElem 0 dir xe)+1) xe )) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		putStrLn "Error to delete path for the user"
		
deleteGroup xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name toCheck = do
	if(null toCheck) then do
		putStrLn "Borrado"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		if( (((toCheck!!0)!!0)!!0) == name) then do
			putStrLn"Found it"
			if( (length(toCheck)) == 1 ) then
				deleteGroup xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name []
			else do 
				deleteGroup xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name (tail(toCheck))
		else do 
			if( (length(toCheck)) == 1) then do 
				deleteGroup xe xa (userGroupList++[head(toCheck)]) userID sdlist vglist lvlist linklist fslist unused name []
			else do
				deleteGroup xe xa (userGroupList++[head(toCheck)]) userID sdlist vglist lvlist linklist fslist unused name (tail(toCheck))

--Username
--UID		path
--Associated primary group:
--Associated secondary groups:
printUserInformation xe xa userGroupList userID name id toCheck i j asPrimary asSecondary sdlist vglist lvlist linklist fslist unused= do
	if(null toCheck) then do
		if (null asSecondary)then do
			putStrLn $ ("Username: "++name++" \n UID:"++id++"\t HomeDirectory:"++"/home/"++name++"\n\n User Primary Group: "++(show(findPrimaryFor name userGroupList [] 0)))
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		else do
			putStrLn $ ("Username: "++name++" \n UID:"++id++"\t HomeDirectory:"++"/home/"++name++"\n\n User Primary Group: "++(show(findPrimaryFor name userGroupList [] 0))++"\n\n User Secondary Groups:"++(show(findSecundaryFor name userGroupList [] 0)))
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused			
	else if ( i <= ( (length((toCheck!!0)!!2)))-1 ) then do		--There are still users as primary in the list of primary, otherwise we move to the users set as secondary
		if ( (((toCheck!!0)!!2)!!i) == name ) then do 								--The user is as Primary in this User Group
			printUserInformation xe xa userGroupList userID name id toCheck (i+1) j (((toCheck!!0)!!2)!!i) asSecondary sdlist vglist lvlist linklist fslist unused
		else do																		--Let's check the next user set as primary
			printUserInformation xe xa userGroupList userID name id toCheck (i+1) j asPrimary asSecondary sdlist vglist lvlist linklist fslist unused
	else if ( j <= (length((toCheck!!0)!!3))-1 ) then do
		if ( (((toCheck!!0)!!3)!!j)==name ) then do 								--The user is as secundary in this User Group
			printUserInformation xe xa userGroupList userID name id toCheck i (j+1) asPrimary (asSecondary++[(((toCheck!!0)!!3)!!j)]) sdlist vglist lvlist linklist fslist unused
		else do																		--Let's check the next user set as secundary
			printUserInformation xe xa userGroupList userID name id toCheck i (j+1) asPrimary asSecondary sdlist vglist lvlist linklist fslist unused
	else do	--This case means we go to the next group
		printUserInformation xe xa (userGroupList++ [head(toCheck)]) userID name id (tail(toCheck)) 0 0 asPrimary asSecondary sdlist vglist lvlist linklist fslist unused
		
		
{------------------------------------------------Functions to logical volume--------------------------------------------------------}		

valueLogicalVolume xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused toCheck size volName vgName = do
	if(null toCheck ) then do
		putStrLn "The volume group doesn't exist"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do 
		if ( f(toCheck!!0) == vgName ) then do
			putStrLn$"espacio disponible :  "++(show((si(toCheck!!0))))
			putStrLn$"espacio que pido :  "++(show(getIntegerFrom size 0 0))
			if( (si(toCheck!!0)) >= (getIntegerFrom size 0 0) ) then do
				--[(f(toCheck!!0)),(s(toCheck!!0)),(t(toCheck!!0)),((fo(toCheck!!0))+1),((s(toCheck!!0))++[volName]),(si(toCheck!!0)), ((l(toCheck!!0))-size) ] 
				body xe xa userGroupList userID sdlist (vglist ++ [( (f(toCheck!!0)),(s(toCheck!!0)),(t(toCheck!!0)),((fo(toCheck!!0))+1),((s(toCheck!!0))++[volName]),(si(toCheck!!0)),  ( (l(toCheck!!0)) - (getIntegerFrom size 0 0))   )]  ++(tail(toCheck))) lvlist linklist fslist unused
			else do
				putStrLn "Not enough space"
				body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		else do
			valueLogicalVolume xe xa userGroupList userID sdlist (vglist++[head(toCheck)]) lvlist linklist fslist unused (tail(toCheck)) size volName vgName

			 
{------------------------------------------------Functions to storage device--------------------------------------------------------}
createStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused newSD newSize i= do
	if( i <= ((length(sdlist))-1)) then do
		if( ((sdlist!!i)!!0) == (newSD) ) then do
			putStrLn $ "The storage device already exists"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused 
		else do		
			createStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused newSD newSize (i+1)
	else do  				--call to create the path		The first name indicates if LVM 	The second one indicates if it belongs to a volume group
		addFiles "d" xe newSD xa userGroupList userID (sdlist++[[newSD,newSize,"null"]]) vglist lvlist linklist fslist unused
		--body xe xa userGroupList userID (sdlist++[[newSD,newSize,"null"]]) vglist lvlist linklist fslist unused

listStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused i = do 
	if(i <= ((length(sdlist))-1) ) then do 
		if( ((sdlist!!i)!!2)=="LVM" ) then do
			putStrLn $ "Disk "++((sdlist!!i)!!0)++": "++((sdlist!!i)!!1)++"MiB Managed by: LVM"
			listStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused (i+1)
		else do
			putStrLn $ "Disk "++((sdlist!!i)!!0)++": "++((sdlist!!i)!!1)
			listStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused (i+1)
	else do	
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	
removeStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused nameToRemove toCheck= do
	if(null toCheck) then do
		--body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		rmFiles xe (nameToRemove) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		if( ((toCheck!!0)!!0) == nameToRemove) then do
			removeStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused nameToRemove (tail(toCheck))
		else
			removeStorageDevice xe xa userGroupList userID (sdlist++[head(toCheck)]) vglist lvlist linklist fslist unused nameToRemove (tail(toCheck))

addLVMtodevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused name toCheck flag= do
	if (null toCheck) then do
		if(flag==True) then do
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused		
		else do
			putStrLn "Not such storage device"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		if( ((toCheck!!0)!!0) == name) then do
			addLVMtodevice xe xa userGroupList (userID) (sdlist++[[ ((toCheck!!0)!!0), ((toCheck!!0)!!1), "LVM" ]]) vglist lvlist linklist fslist unused name (tail(toCheck)) True
		else do
			addLVMtodevice xe xa userGroupList (userID) (sdlist++[head(toCheck)]) vglist lvlist linklist fslist unused name (tail(toCheck)) flag

removeSD xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused nameToRemove toCheck= do
	if(null toCheck) then do
		True
	else do
		if( ((toCheck!!0)!!0) == nameToRemove) then do
			removeSD xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused nameToRemove (tail(toCheck))
		else
			removeSD xe xa userGroupList userID (sdlist++[head(toCheck)]) vglist lvlist linklist fslist unused nameToRemove (tail(toCheck))

isManagedByLVM sdlist sdname = do
	if(null sdlist) then do
		False		
	else do 
		if( ((sdlist!!0)!!0) == sdname) then do
			if( ((sdlist!!0)!!2) == "LVM" ) then do
				True
			else do 
				False
		else do
			isManagedByLVM (tail(sdlist)) sdname

getSize sdlist name = do
	if(null sdlist) then do
		0
	else
		if ( ((sdlist!!0)!!0) == name ) then do
			((sdlist!!0)!!1)
		else do
			getSize (tail(sdlist)) name

			
{------------------------------------------------Functions to manage Files--------------------------------------------------------}
echoFile xe xa doc dir userGroupList userID sdlist vglist lvlist linklist fslist unused= do
	if (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "-") then do
		--putStrLn $ show((getElem 0 dir xe))
		let tuple = (f(xe !! (getElem 0 dir xe)),s(xe !! (getElem 0 dir xe)),t(xe !! (getElem 0 dir xe)),fo(xe !! (getElem 0 dir xe)),fi(xe !! (getElem 0 dir xe)),doc,l(xe !! (getElem 0 dir xe)))
		body ((fhalf 0 xe (getElem 0 dir xe) []) ++ [tuple] ++(shalf ((getElem 0 dir xe)+1) xe (getElem 0 dir xe) []))  xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		putStrLn "Fallo"
fhalf cont xe ind final= do
	if (cont < ind)then do
		fhalf (cont+1) xe ind (final ++ [xe!!cont])
	else do
		final
shalf cont xe ind final= do
	if (cont < (length xe))then do
		fhalf (cont+1) xe ind (final ++ [xe!!cont])
	else do
		final
catFile xe xa dir sdlist vglist lvlist linklist fslist unused=do
	if (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "-") then do
		putStrLn $ show (si(xe !! (getElem 0 dir xe) ))
	else
		putStrLn "No existe"
	
{------------------------------------------------Functions to manage the Path-----------------------------------------------------}

rmFiles xe dir xa userGroupList userID sdlist vglist lvlist linklist fslist unused= do
	putStrLn $ show (isNow 0 dir xe)
	putStrLn $ show (isEmpty xe 0 dir)
	if (isNow 0 dir xe) == False then do
		putStrLn "El archivo no existe"
	else if isEmpty xe 0 dir == False then do
		putStrLn "La carpeta no esta vacia"
		
	else if (isNow 0 dir xe) && isEmpty xe 0 dir then do
		body ((take ((getElem 0 dir xe)) xe )++(drop ((getElem 0 dir xe)+1) xe )) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		putStrLn "No Se"

	

isEmpty xe cont dir  = do
	
	if cont < length xe then do
		if (dir `isInfixOf` s(xe !! cont)) && (("/"++dir) /= s(xe !! cont)) then do
			False
		else do
			isEmpty xe (cont+1) dir
	else
		True
	


addFiles mode xe add xa userGroupList userID sdlist vglist lvlist linklist fslist unused = do
	now <- getCurrentTime
	time <- getCurrentTimeZone
	if isNow 0 add xe then do
		--putStrLn $"Error creating the path: '"++(show(add))++"' already exist"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if ("/" `isInfixOf` add )then do
		splitAdd mode mode 0 (splitOn "/" add) xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if (f (head xe )) == "/" then do
		body (xe ++ [(add ,f(head xe) ++ add,getDate now,"time","root","",mode)]) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do 
		body (xe ++ [(add, s(head xe)++"/" ++ add,getDate now,"Hora","root","",mode)]) xa userGroupList userID sdlist vglist lvlist linklist fslist unused

addFiles2 mode modeT cont dir xe add xa userGroupList userID sdlist vglist lvlist linklist fslist unused= do
	now <- getCurrentTime
	if (f (head xe )) == "/" then do
		--putStrLn "d"
		splitAdd mode modeT (cont + 1) dir (xe ++ [(last(splitOn "/" add) ,f(head xe) ++ add,getDate now,"time","root","",mode)]) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		splitAdd mode modeT (cont + 1) dir (xe ++ [(last(splitOn "/" add) ,f(head xe) ++"/"++ add,getDate now,"time","root","",mode)]) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		
splitAdd mode modeT cont dir xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused= do
	if (cont == 0) then do
		--putStrLn "s"
		addFiles2 "d" modeT cont dir xe (dir !! 0) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if (cont == (length dir)-1) && modeT == "-" then do
		addFiles2 modeT modeT cont dir xe (f(last xe)++"/"++ (dir !! cont)) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if (cont < length dir) && (cont /= 0) then do
		addFiles2 "d" modeT cont dir xe (f(last xe)++"/"++ (dir !! cont)) xa userGroupList userID sdlist vglist lvlist linklist fslist unused	
	else
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused


listFiles cont  xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused= do
	
	if (cont < length xe) then do
		if (f(head xe ) `isInfixOf` s(xe !! cont)) && (f( xe !! 2) /= s(xe !! (cont)))  then do
			putStrLn $ f(xe !! cont)
			listFiles (cont + 1) xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		else do
			listFiles (cont + 1 ) xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do 
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused

moveDirectory dir xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused= do
	if dir == ".." then do
		putStrLn "retroceder"
	else if dir == "/" then do
		body ((xe!!1):tail xe) xa userGroupList userID sdlist vglist lvlist linklist fslist unused	
	else if (s(head xe) == "/") && (isNow 0 dir xe) && (l(xe !! (getElem 0 dir xe)) == "d") then do
		body ((xe !! getElem 0 dir xe):tail xe) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if  (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "d") then do
		body ((xe !! ((getElem 0 dir xe))):tail xe) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if  (isNow 0 (dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "d") then do
		body ((xe !! ((getElem 0 dir xe))):tail xe) xa userGroupList userID sdlist vglist lvlist linklist fslist unused
	else if  (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "-") then do
		putStrLn "Archivo"
	else do 
		putStrLn "No existe"
		body xe xa userGroupList userID sdlist  vglist lvlist linklist fslist unused

	
getElem cont dir xe = do
	if (cont < length xe )then do
		if (s (head xe ) ++ dir) == s(xe !! cont) then do 
			cont
		else if (s (head xe )++"/"++ dir) == s(xe !! cont) then do 
			cont
		else do
			getElem (cont +1) dir xe 
	else do
		cont
		
	
isNow cont dir xe = do
	if (cont < length xe )then do
		if ((s (head xe ) ++ dir) == s(xe !! cont)) || ((s (head xe ) ++"/"++ dir) == s(xe !! cont)) then do 
			True
		else do
			isNow (cont +1) dir xe 
	else 
		False

getDate now = do
	show(sd(toGregorian $ utctDay now))++"/"++show(td(toGregorian $ utctDay now))++"/"++show(fd(toGregorian $ utctDay now))
getTime now time = do
	localTimeOfDay $ utcToLocalTime time now
	




	

{----------------------------------------------------------End Path----------------------------------------------------------------}
--Begin Device Storage
createdDev size dir xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused= do 
	if (isNow2 0 dir xe) then do
		putStrLn "SIPP" 
		body xe ((dir,size,dir):xa) userGroupList userID sdlist vglist lvlist linklist fslist unused
	else do
		putStrLn "El directorio no existe"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist unused
		


		

isNow2 cont dir xe = do	
	if (cont < length xe )then do
		if (dir) == s(xe !! cont) then do 
			True
		else do
			isNow2 (cont +1) dir xe 
	else 
		False




--End Device Storage





f (a, _, _, _, _, _, _) = a
s (_, a, _, _, _, _, _) = a
t (_, _, a, _, _, _, _) = a
fo (_, _, _, a, _, _, _) = a
fi(_, _, _, _, a, _, _) = a
si(_, _, _, _, _, a, _) = a
l (_, _, _, _, _, _, a) = a
fd (a, _, _) = a
sd (_, a, _) = a
td (_, _, a) = a


numberCheck txt = do
	if(null txt) then
		True
	else if( (txt!!0)=='0' || (txt!!0)=='1' || (txt!!0)=='2' || (txt!!0)=='3' || (txt!!0)=='4' || (txt!!0)=='5' || (txt!!0)=='6' || (txt!!0)=='7' || (txt!!0)=='8' || (txt!!0)=='9') then do
  		if(length(txt)==1) then
   			numberCheck []
  		else 
   			numberCheck (tail(txt))
 	else 
  		False
		  
getIntegerFrom toCheck power int = do
	if(null toCheck) then do
		int
	else if( (last(toCheck)) =='0') then do
		getIntegerFrom (init(toCheck)) (power+1) (int) 
	else if( (last(toCheck)) =='1') then do
		getIntegerFrom (init(toCheck)) (power+1) ((1)*(10^power)+int)
	else if( (last(toCheck)) =='2') then do
		getIntegerFrom (init(toCheck)) (power+1) ((2)*(10^power)+int)
	else if( (last(toCheck)) =='3') then do
		getIntegerFrom (init(toCheck)) (power+1) ((3)*(10^power)+int)
	else if( (last(toCheck)) =='4') then do
		getIntegerFrom (init(toCheck)) (power+1) ((4)*(10^power)+int)
	else if( (last(toCheck)) =='5') then do
		getIntegerFrom (init(toCheck)) (power+1) ((5)*(10^power)+int)
	else if( (last(toCheck)) =='6') then do
		getIntegerFrom (init(toCheck)) (power+1) ((6)*(10^power)+int)
	else if( (last(toCheck)) =='7') then do
		getIntegerFrom (init(toCheck)) (power+1) ((7)*(10^power)+int)
	else if( (last(toCheck)) =='8') then do
		getIntegerFrom (init(toCheck)) (power+1) ((8)*(10^power)+int)
	else if( (last(toCheck)) =='9') then do
		getIntegerFrom (init(toCheck)) (power+1) ((9)*(10^power)+int)
	else 
		0