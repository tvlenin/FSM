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

body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist = do 

	--putStrLn $ show linklist
	--putStrLn $ show vglist
	--putStrLn $ show xe
	--putStrLn $ show vglist
	--putStrLn $ show sdlist


	op <- getLine
	if  (op == "") then			--The sinstaxis must be correct
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	
	else if  head(words(op)) == "showall" && length(words(op))==1 then do			--The sinstaxis must be correct
		--addUserGroup xe xa userID userGroupList [[(words(op)!!1)],[ show ((read ((((last(userGroupList))!!1)!!0)) :: Integer)+1) ],[],[]] sdlist vglist lvlist linklist fslist mplist	--Call the function to add the new group
		putStrLn$"---------------------------------------------------------------------------------------------------"
		putStrLn$"Group name \t\t\t Gorup id \t\t\t Users as Primary \t\t\t Users as Secondary"
		showAllStart xe xa [] userID sdlist vglist lvlist linklist fslist mplist userGroupList
		
	{-	Command : #groupadd <nombre del grupo> 
		Here is possible to add a new user group-}	
	else if  head(words(op)) == "groupadd" && length(words(op))==2 then do			--The sinstaxis must be correct
		--addUserGroup xe xa userID userGroupList [[(words(op)!!1)],[ show ((read ((((last(userGroupList))!!1)!!0)) :: Integer)+1) ],[],[]] sdlist vglist lvlist linklist fslist mplist	--Call the function to add the new group
		addUserGroup xe xa userID userGroupList [[(words(op)!!1)],[ show ( (getIntegerFrom ((((last(userGroupList))!!1)!!0)) 0 0) +1) ],[],[]] sdlist vglist lvlist linklist fslist mplist	--Call the function to add the new group
 	else if (head(words(op)) == "show") && (head(tail(words(op))) == "groups") && length(words(op))==2 then do
		putStrLn $ "GroupName\t\tGID\t\tAssociated Primary Users \t\t AssociatedAsSecondaryUsers"
		showAllgroups xe xa userID userGroupList 0 sdlist vglist lvlist linklist fslist mplist

	else if (head(words(op)) == "show") && (head(tail(words(op))) == "users") && length(words(op))==2 then do
		putStrLn $ "UserName \t\t UID \t\t PrimaryGroup \t\t SecondaryGroups \t\t HomeDirectory"
		showAllUsers xe xa userID userGroupList userID userGroupList sdlist vglist lvlist linklist fslist mplist

	{- 	Command : # useradd -g primaryGroup [-G secondaryGroup1 secondaryGroup2] userName
		An user must have an primary group associated, the secondary ones are optional-}
	else if head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))==4 || ( head(words(op)) == "useradd" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G" ) then do
		createNewUser xe xa userID [] userGroupList (init(tail(tail(words(op))))) (last(tail(tail(words(op)))))	0 sdlist vglist lvlist linklist fslist mplist

	else if ( head(words(op))=="usermod" && (words(op)!!1) =="-g" && length(words(op))==4) then do 
		modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist (last(words(op))) (words(op)!!2) []
		
	else if( head(words(op)) == "usermod" && (words(op)!!1) =="-G" && length(words(op))>=4 ) then do 
		modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist (last(words(op))) "none" (tail(tail((init(words(op))))))
		
	else if( head(words(op)) == "usermod" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G"  ) then do
		modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist (last(words(op))) ((words(op))!!2) (tail(tail(tail(tail(init(words(op)))))))
		
--	else if ( head(words(op)) == "usermod" && (words(op)!!1) =="-g" && length(words(op))>=6 && (words(op)!!3)=="-G")) then do
--		modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist (tail(words(op))) ()  
																								-- ^ name 
	{- 	Command : # finger <username>
	Displays information about the specified user 
	-}
	else if  head(words(op)) == "finger" && length(words(op))==2 then do			--The sinstaxis must be correct
		findUser xe xa userID userGroupList ((words(op))!!1) 0 sdlist vglist lvlist linklist fslist mplist

	else if  head(words(op)) == "userdel" && length(words(op))==2 then do			--The sinstaxis must be correct
		if( ((words(op))!!1) == "root") then do
			putStrLn$"Well well you are trying to delete root, really?"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		else do 
			deleteUser xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist ((words(op))!!1) 0
		--findUser xe xa userID userGroupList ((words(op))!!1) 0 sdlist vglist lvlist linklist fslist mplist
		
	else if  head(words(op)) == "groupdel" && length(words(op))==2 then do			--The sinstaxis must be correct
		deleteGroup xe xa [] userID sdlist vglist lvlist linklist fslist mplist ((words(op))!!1) userGroupList
				
	else if ( (head(words(op))=="createdev") && (((words(op))!!1)=="-s") && (length(words(op))==4) && (numberCheck ((words(op))!!2))) then do
		createStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist ((words(op))!!3) ((words(op))!!2) 0
	else if ( (head(words(op))=="createdev") && (((words(op))!!1)=="-s") && (length(words(op))==4) && (not(numberCheck ((words(op))!!2)))) then do
		putStrLn$"Size must be an integer"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		
	else if ( (head(words(op))=="fdisk") && (length(words(op))==2) && (((words(op))!!1)=="-l") ) then do
		--putStrLn$ ""
		listStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist 0
		
	else if ( (head(words(op))=="rmdev") && (length(words(op))==2)) then do
		if( not (haveStorage 0 vglist ((words(op))!!1)) ) then do
			removeStorageDevice xe xa userGroupList userID [] vglist lvlist linklist fslist mplist ((words(op))!!1) sdlist
		else 
			putStrLn $"The storage device can't be deleted because it belongs to a volume group"
			
	else if ( (head(words(op))=="pvcreate") && (length(words(op))==2) ) then do
		addLVMtodevice  xe xa userGroupList userID [] vglist lvlist linklist fslist mplist ((words(op))!!1) sdlist False
		
	else if ( (length(words(op)))==4 && ((words(op))!!0)=="mkfs"  && ((words(op))!!1)=="-t" ) then do
		createFileSystem xe xa userGroupList userID sdlist vglist lvlist linklist [] mplist ((words(op))!!2) ((words(op))!!3) fslist
		
	else if ( (length(words(op)))==3 && ((words(op))!!0)=="mount" ) then do
		mountPoint xe xa userGroupList userID sdlist vglist lvlist linklist [] mplist ((words(op))!!1) ((words(op))!!2) fslist
		
	else if ( (length(words(op)))==2 && ((words(op))!!0)=="df"  && ((words(op))!!1)=="-h" ) then do
		putStrLn$"FileSystem \t\t\t type \t\t\t Mounted on"
		showFileSystemOut xe xa userGroupList userID sdlist vglist lvlist linklist [] mplist fslist
		
	
	else if (head(words(op)) == "mkdir") &&(head(tail(words(op))) == "-p") && ("/" `isInfixOf` (last(words(op))) ) && (length(words(op))) == 3 then do
		addFiles "d" xe (last(words(op))) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if (head(words(op)) == "mkdir")&& ("/" `isInfixOf` (last(words(op))) ) == False  && (length(words(op))) == 2 then do
		addFiles "d" xe (last(words(op))) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if (head(words(op)) == "rmdir") && (length(words(op))) ==2  then do
		rmFiles xe (last(words(op))) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if (head(words(op)) == "rmdir") &&(head(tail(words(op)))) == "-rf" && (length(words(op))) == 3  then do
		rmFiles xe (last(words(op))) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		
		
	else if (head(words(op))=="touch") && (length(words(op))) == 2 then do
		addFiles "-" xe (last(words(op))) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	
	else if (head(words(op)) == "echo") && (head(tail(tail(words(op)))) == ">>") && (length(words(op))) == 4 then do
		echoFile xe xa (head(tail(words(op)))) (last(words(op))) userGroupList userID sdlist vglist lvlist linklist fslist mplist
	
	else if (head(words(op))) == "cat" && (length(words(op))) == 2 then do
		catFile xe xa (last(words(op))) sdlist vglist lvlist linklist fslist mplist
		
	else if (head(words(op)) == "ls") && (length(words(op))) == 2 then do
		putStrLn "con op"
		listFiles (last(words(op))) 0 xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	
	else if (head(words(op)) == "ls") && (length(words(op))) == 1 then do
		putStrLn "sin op"
		listFiles "none" 0 xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	
	else if (head(words(op)) == "cd") && length(words(op))==2 then do
		if (isLink 0 (head(tail(words(op)))) linklist) && (fi(linklist!!(getLink 0 (head(tail(words(op)))) linklist) ) == "d") then do
			putStrLn "moviendo"
			moveDirectory (s(linklist!!(getLink 0 (head(tail(words(op)))) linklist) )) xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		else do
			moveDirectory (last(words(op))) xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	
	
	
		
	else if ( ((length(words(op))) >= 3 ) && (((words(op))!!0) == "vgcreate") ) then do
		
		if ( (alreadyVG 0 (tail(tail(words(op)))) xe) && (listHasLVM 0  (tail(tail(words(op)))) sdlist ) ) then do
			body xe xa userGroupList userID sdlist (addVolumeGroups 0 vglist (words(op)!!1) (tail(tail(words(op)))) (getSumOfStorage sdlist (tail(tail(words(op)))) 0 ) ) lvlist linklist fslist mplist		
			--body xe xa userGroupList userID sdlist (addVolumeGroups 0 vglist (words(op)!!1) (tail(tail(words(op)))) 100 ) lvlist linklist fslist mplist		
		else if (not (alreadyVG 0 (tail(tail(words(op)))) xe) ) then do 
			putStrLn "Error creating VG, A volume does not exist"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		-- add the error for a volume that belongs to a VG
		else do
			putStrLn "Error creating VG, a volume has not a LVM"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist

	else if  ((length(words(op))) == 3 ) && (((words(op))!!0) == "vgextend")  then do
		putStrLn $ (show ((getSize sdlist (last(words(op))))))
		body xe xa userGroupList userID sdlist (vgExtend 0 vglist ((words(op))!!1)  (last(words(op)))   (getSize sdlist (last(words(op))))    ) lvlist linklist fslist mplist 
	else if  ((length(words(op))) == 3 ) && (((words(op))!!0) == "vgreduce")  then do
		putStrLn $ (show ((getSize sdlist (last(words(op))))))
		body xe xa userGroupList userID sdlist (vgReduce 0 vglist ((words(op))!!1)  (last(words(op)))   (getSize sdlist (last(words(op))))    ) lvlist linklist fslist mplist 
	
		
		
		--body xe xa userGroupList userID sdlist (vgExtend 0 vglist ((words(op))!!1) (last(words(op)))(getSize sdlist (last(words(op))) ) )  lvlist linklist fslist mplist
	else if ( (head(words(op)) == "lvcreate") && (length(words(op)) == 6) && ((words(op)!!1) == "-L") && ((words(op)!!3) == "-n")) then do 
		if((numberCheck ((words(op))!!2))) then do
			valueLogicalVolume xe xa userGroupList userID sdlist [] lvlist linklist fslist mplist vglist ((words(op))!!2) ((words(op))!!4) ((words(op))!!5)
		else do 
			putStrLn$"The size must be an integer"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	
	else if ( (length(words(op)))==1 && ((((words(op))!!0)=="vgdisplay" ) || ((words(op))!!0)=="vgs" )) then do
		showInfoOfVG xe xa userGroupList userID sdlist [] lvlist linklist fslist mplist vglist
		
	else if ( (length(words(op)))==2 && (((words(op))!!0)=="vgdisplay"|| ((words(op))!!0)=="vgs" )  && (((words(op))!!1)=="-v") ) then do
		showMoreInfoOfVG xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist	vglist
						
	else if (head(words(op)) == "ln") && (head(tail(words(op))) == "-s") && length(words(op))==4 then do
		putStrLn $ show (head(tail(tail(words(op))))) 
		if (isNow 0 (head(tail(tail(words(op))))) xe) then do 
			addFiles "-" xe (head(tail(tail(tail(words(op)))))) xa userGroupList userID sdlist vglist lvlist ((linkCreate (head(tail(tail(words(op))))) (head(tail(tail(tail(words(op)))))) "-"):linklist) fslist mplist
			--body xe xa userGroupList userID sdlist vglist lvlist ((linkCreate (head(tail(tail(words(op))))) (head(tail(tail(tail(words(op)))))) "d"):linklist) fslist mplist 
		else do
			putStrLn "Error, the file does no exist"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	
	else if (head(words(op)) == "echo") && length(words(op))==3 then do
		if (isLink 0 (head(tail(words(op)))) linklist) && (fi(linklist!!(getLink 0 (head(tail(words(op)))) linklist) ) == "-") then do
			putStrLn "entra al echo"
			echoFile xe xa (last(words(op))) (s(linklist!!(getLink 0 (head(tail(words(op)))) linklist) )) userGroupList userID sdlist vglist lvlist linklist fslist mplist
		else do
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		putStrLn $ ("Sintaxis error")
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist

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
	(volumeList ++ [(vgName,listpv,(length (listpv)), 0,[],size,size)])
	
	
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
getVG cont vgname vglist = do
	if(cont < (length vglist) ) then do
		if f(vglist !! cont) == vgname then do
			cont 
		else do
			getVG (cont + 1) vgname vglist
	else do
		cont
	
vgExtend cont vglist vgname sdName size = do
	if (cont < (length vglist)) then do
		if (f(vglist!!cont)) == vgname then do
			(take ((getVG 0 vgname vglist)) vglist ) ++ [(vgname,s(vglist!!cont) ++ [sdName],t(vglist!!cont) + 1,fo(vglist!!cont),fi(vglist!!cont),si(vglist!!cont) + size,l(vglist!!cont) + size  )]++drop ((getVG 0 vgname vglist)+1) vglist
		else do
			vgExtend (cont + 1) vglist vgname sdName size
	else do
		(take ((getVG 0 vgname vglist)) vglist ) ++ [(vgname,s(vglist!!cont) ++ [sdName],t(vglist!!cont) + 1,fo(vglist!!cont),fi(vglist!!cont),si(vglist!!cont) + size,l(vglist!!cont) + size  )]++drop ((getVG 0 vgname vglist)+1) vglist

vgReduce cont vglist vgname sdName size = do
	if (cont < (length vglist)) then do
		
		
		if(f(vglist!!cont)) == vgname then do 
			(take ((getVG 0 vgname vglist)) vglist ) ++ [(vgname,getNewSd (s(vglist!!cont)) sdName ,t(vglist!!cont) - 1,fo(vglist!!cont),fi(vglist!!cont),si(vglist!!cont) - size,l(vglist!!cont) - size  )]++drop ((getVG 0 vgname vglist)+1) vglist
		else do
			vgReduce (cont + 1) vglist vgname sdName size
	else do
		(take ((getVG 0 vgname vglist)) vglist ) ++ [(vgname,getNewSd (s(vglist!!cont)) sdName ,t(vglist!!cont) - 1,fo(vglist!!cont),fi(vglist!!cont),si(vglist!!cont) - size,l(vglist!!cont) - size  )]++drop ((getVG 0 vgname vglist)+1) vglist

getNewSd listsd sdName = do 
	(take (getSdIndex 0 listsd sdName) listsd ) ++ (drop ((getSdIndex 0 listsd sdName)+1) listsd)
getSdIndex cont listsd sdName = do
	if (cont < (length listsd)) then do
		if(listsd!!cont) == sdName then do
			cont
		else
			getSdIndex (cont+1) listsd sdName
	else 
		cont 
	

{-------------------------------User---Groups----------------------------------------}
printuserg xe xa userID userGroupList sdlist vglist lvlist linklist fslist mplist= do
	putStrLn $ (show(userGroupList))
	body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
{-
	To add the user group with all its information-}
addUserGroup xe xa  userID userGroupList add sdlist vglist lvlist linklist fslist mplist= do
	checkAndAdd xe xa userID userGroupList 0 add sdlist vglist lvlist linklist fslist mplist
	
checkAndAdd xe xa userID userGroupList i add sdlist vglist lvlist linklist fslist mplist= do
	if( i<= (length(userGroupList)-1)) then do
		if ( (((userGroupList!!i)!!0)!!0) == ((add!!0)!!0)) then do		--this means the user group already exits
			putStrLn $ "The user group already exits"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		else do
			checkAndAdd xe xa userID userGroupList (i+1) add sdlist vglist lvlist linklist fslist mplist					--check the name of the next user group			
	else do											--The user group does not exits
--		putStrLn $ (show userGroupList)
--		putStrLn $ ("agredando!: "++show(add))
		body xe xa (userGroupList++[add]) userID sdlist vglist lvlist linklist fslist mplist
		
{-Here is the display of GroupName -> GroupId -> AssociatedUsers -> AssociatedSecondaryUsers
-}
showAllgroups xe xa userID userGroupList i sdlist vglist lvlist linklist fslist mplist= do
	if( i<= (length(userGroupList)-1) ) then do				--there are still groups to print			
		--putStrLn $ (show ( (((userGroupList!!i)!!0)!!0) ++ "\t" ++ (((userGroupList!!i)!!1)!!0) ++"\t" ++((userGroupList!!i)!!2) ++ "\t"++ ((userGroupList!!i)!!3) ) )
		putStrLn $ (( ((((userGroupList!!i)!!0)!!0)) ++ "\t\t\t" ++ (((userGroupList!!i)!!1)!!0) ++"\t\t\t" ++(show((userGroupList!!i)!!2)) ++ "\t\t\t" ++ (show((userGroupList!!i)!!3)) ) )		
		showAllgroups xe xa userID userGroupList (i+1) sdlist vglist lvlist linklist fslist mplist
	else do
		body xe xa userGroupList userID	sdlist vglist lvlist linklist fslist mplist
	
	
showAllStart xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck = do 
	if(null toCheck) then do
		putStrLn$"---------------------------------------------------------------------------------------------------"
		putStrLn$"Username \t\t\t userID \t\t\t LVM "
		showAllSecondUsers xe xa userGroupList [] sdlist vglist lvlist linklist fslist mplist userID		
	else do
		putStrLn$(show(((toCheck!!0)!!0)!!0))++"\t\t\t\t"++(show(((toCheck!!0)!!1)!!0))++"\t\t\t\t"++(show(((toCheck!!0)!!2)))++"\t\t\t"++(show(((toCheck!!0)!!3)))
		showAllStart xe xa (userGroupList++[head(toCheck)]) userID sdlist vglist lvlist linklist fslist mplist (tail(toCheck))

showAllSecondUsers xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck = do
	if(null toCheck) then do 
		putStrLn$"---------------------------------------------------------------------------------------------------"
		putStrLn$"Storage device name \t\t\t Size \t\t\t Managed by LVM"
		showAllDeviceFirst xe xa userGroupList userID [] vglist lvlist linklist fslist mplist sdlist
	else do 
		--putStrLn$show(head(toCheck))
		putStrLn$ (show((toCheck!!0)!!0))++"\t\t\t"++(show((toCheck!!0)!!1)) -- ++"\t\t\t" ++(show((toCheck!!0)!!2))
		if(length(toCheck)==1) then do
			showAllSecondUsers xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist mplist []
		else do
			showAllSecondUsers xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist mplist (tail(toCheck))

showAllDeviceFirst xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck = do
	if(null toCheck) then do 
		putStrLn$"---------------------------------------------------------------------------------------------------"
		putStrLn$"Volume group name \t Phsysical Storages \t N Phy.Stgs \t\t N Logical Srgs \t Logical Storages \t\t Total size \t\t\t Free size"
		showAllVolumeGroup xe xa userGroupList userID sdlist [] lvlist linklist fslist mplist vglist
	else do
		putStrLn$ (show((toCheck!!0)!!0))++"\t\t\t\t"++ (show((toCheck!!0)!!1))++"\t\t\t\t"++(show((toCheck!!0)!!2))
		showAllDeviceFirst xe xa userGroupList userID (sdlist++[head(toCheck)]) vglist lvlist linklist fslist mplist (tail(toCheck))

showAllVolumeGroup xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck = do
	if (null toCheck) then do 
		putStrLn$"---------------------------------------------------------------------------------------------------"
		putStrLn$"File System \t Type \t mount point"
		showAllFSInfo xe xa userGroupList userID sdlist vglist lvlist linklist [] mplist fslist
	else do
		putStrLn$( (show(f(toCheck!!0)))++"\t\t\t"++ (show(s(toCheck!!0)))++"\t\t\t\t"++(show(t(toCheck!!0)))++"\t\t\t"++(show(fo(toCheck!!0)))++"\t\t"++(show(fi(toCheck!!0)))++"\t\t\t"++(show(si(toCheck!!0)))++"\t\t\t\t"++(show(l(toCheck!!0))))

		
showAllFSInfo xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck = do
	if(null toCheck) then do 
		putStrLn"----"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		putStrLn$ (show((toCheck!!0)!!0))++"\t\t" ++(show((toCheck!!0)!!1))++"\t\t"++(show((toCheck!!0)!!2))
		showAllFSInfo xe xa userGroupList userID sdlist vglist lvlist linklist (fslist++[head(toCheck)]) mplist (tail(toCheck))
		
		
{-This prints all the required information about all the users.
-}
showAllUsers xe xa userID userGroupList users groups sdlist vglist lvlist linklist fslist mplist = do
	if ( null users ) then do
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		putStrLn $ ((users!!0)!!0)++"\t\t\t"++((users!!0)!!1)++"\t\t\t"++show(findPrimaryFor ((users!!0)!!0) groups [] 0)++"\t\t\t"++show(findSecundaryFor ((users!!0)!!0) groups [] 0)++"\t\t\t"++" /home/"++((users!!0)!!0)
		if ((length(users))==1) then do
			showAllUsers xe xa userID userGroupList [] groups sdlist vglist lvlist linklist fslist mplist
		else do
			showAllUsers xe xa userID userGroupList (tail(users)) groups sdlist vglist lvlist linklist fslist mplist 
		
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
createNewUser xe xa userID userGroupList toCheck args name j sdlist vglist lvlist linklist fslist mplist= do	
	if ( (length(toCheck) == 0)) then do 							--When this is True, means we can created this user
		if(length(args)==1) then do									--this occurs when there are no secondary groups(-G)
			addNewUserPrimary xe xa userID [] userGroupList 0 (head(args)) [] name sdlist vglist lvlist linklist fslist mplist
		else do														--if there are secondary groups, I extract the -G term
			addNewUserPrimary xe xa userID [] userGroupList 0 (head(args)) (tail(tail(args))) name sdlist vglist lvlist linklist fslist mplist
	else do		
		if (not (elem name ((toCheck!!0)!!2))) then do
			createNewUser xe xa userID (userGroupList++[head(toCheck)]) (tail(toCheck)) args name j sdlist vglist lvlist linklist fslist mplist
		else do
			putStrLn "The user exists"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist

addNewUserPrimary xe xa userID userGroupList toCheck j primary secondary name sdlist vglist lvlist linklist fslist mplist = do	
	if ((length(toCheck))==0) then do 							--We checked the whole group names but failed
		putStrLn "userAdd -> The primary group does not exits"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		if ( (((toCheck!!0)!!0)!!0) ==  primary ) then do	--This means we found the correct group to add the user as primary 			
			addNewUserSecondary xe xa (userID++[[name,  show((read((last(userID))!!1)::Integer)+1)  ]]) [] (userGroupList ++ [ [((toCheck!!0)!!0)] ++ [((toCheck!!0)!!1)] ++ [(((toCheck!!0)!!2)++[name])] ++ [((toCheck!!0)!!3)] ] ++ (tail(toCheck)) ) 0 0 (secondary) name sdlist vglist lvlist linklist fslist mplist
		else do
			--2 casos:
			--a. j ya se va a salir del grupo, significa que paso de grupo y lo agrego a userGroupList
			--b. aumentar j para revisar otro usuario dentro del mismo userGroupList
			if( (j <= (length (((toCheck!!0)!!2))) -1 ) ) then do
				addNewUserPrimary xe xa userID userGroupList toCheck (j+1) primary secondary name sdlist vglist lvlist linklist fslist mplist-- To compare the next one, inside the same group
			else do
				if(length(toCheck)==1) then do
					addNewUserPrimary xe xa userID (userGroupList++[(head(toCheck))]) [] 0 primary secondary name sdlist vglist lvlist linklist fslist mplist
				else do 
					addNewUserPrimary xe xa userID (userGroupList++[(head(toCheck))]) (tail(toCheck)) 0 primary secondary name sdlist vglist lvlist linklist fslist mplist
			  		
addNewUserSecondary xe xa userID userGroupList toCheck i k secondary name sdlist vglist lvlist linklist fslist mplist= do
	if( null secondary) then do
		--body xe xa (userGroupList++toCheck) userID
		addFiles "d" xe ("home/"++name) xa (userGroupList++toCheck) userID sdlist vglist lvlist linklist fslist mplist 	
	else if (null toCheck) then do
		putStrLn $ "A secondary does not exist"++name		
		body xe xa (userGroupList++toCheck) userID sdlist vglist lvlist linklist fslist mplist
	else do
		if( i <= ((length(toCheck))-1) ) then do
			if(k <= (length(secondary))-1) then do
				if ( elem (secondary!!k) (((toCheck!!i)!!0)) ) then do	--We found the right place to add it as secondary											
					concatenate xe xa userID userGroupList [] toCheck secondary name sdlist	 vglist lvlist linklist fslist mplist
				else do
					addNewUserSecondary xe xa userID userGroupList toCheck i (k+1) secondary name sdlist vglist lvlist linklist fslist mplist
			else do
				addNewUserSecondary xe xa userID userGroupList toCheck (i+1) (0) secondary name sdlist vglist lvlist linklist fslist mplist
		else do																						--The secondary does not exist
			(addNewUserSecondary xe xa userID (userGroupList++toCheck) [] 0 0 secondary name) sdlist vglist lvlist linklist fslist mplist

concatenate xe xa userID userGroupList answer toCheck secondary name sdlist vglist lvlist linklist fslist mplist=
	if (null toCheck && null secondary) then do
		addNewUserSecondary xe xa userID userGroupList (answer) 0 0 [] name sdlist vglist lvlist linklist fslist mplist
	else if ( null toCheck) then do													--done checking, go back to add as secondary
		addNewUserSecondary xe xa userID userGroupList (answer++toCheck) 0 0 secondary name	sdlist vglist lvlist linklist fslist mplist
	else if(null secondary) then do
		addNewUserSecondary xe xa userID userGroupList (answer++toCheck) 0 0 [] name sdlist vglist lvlist linklist fslist mplist
	else do																		--there are still elements to concatenate
		if( (((toCheck!!0)!!0)!!0) == (secondary!!0) ) then do 							--This is the place to add
			if(length(secondary)<=1 && (length(toCheck))<=1) then do
				concatenate xe xa userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) [] [] name sdlist vglist lvlist linklist fslist mplist
			else if (length(secondary)<=1) then do
				concatenate xe xa userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) (tail(toCheck)) [] name sdlist vglist lvlist linklist fslist mplist
			else if ((length(toCheck))<=1) then do
				concatenate xe xa userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) [] (tail(secondary)) name sdlist vglist lvlist linklist fslist mplist
			else
				concatenate xe xa userID userGroupList (((answer++[[[(((toCheck!!0)!!0)!!0)],[(((toCheck!!0)!!1)!!0)],(((toCheck!!0)!!2)),(((toCheck!!0)!!3)++[name])]]))) (tail(toCheck)) (tail(secondary)) name sdlist vglist lvlist linklist fslist mplist
		else do
			concatenate xe xa userID userGroupList (answer++[head(toCheck)]) (tail(toCheck)) secondary name	sdlist vglist lvlist linklist fslist mplist
	
	
	
	
		
{-
-}
modifyInformation xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name argsPrimary argsSecundary = do
	dellUserModify xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name 0 argsPrimary argsSecundary

dellUserModify xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name i argsPrimary argsSecundary= do
	if (i > ((length(userID)-1))) then do
		putStrLn$"The user does not exist"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do 
		if( name == ((userID!!i)!!0)) then do -- the user exist
			deletePath2 xe ("home/"++((userID!!i)!!0)) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name argsPrimary argsSecundary
			--dellUserModify2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist mplist name userID argsPrimary argsSecundary
		else do
			dellUserModify xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name (i+1) argsPrimary argsSecundary
			
dellUserModify2 xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name toCheck argsPrimary argsSecundary = do
	if(null toCheck) then do
		deleteUserFromGroup2 xe xa [] (userID) sdlist vglist lvlist linklist fslist mplist userGroupList name argsPrimary argsSecundary-- Erase from the user groups
	else do
		if (name == ((toCheck!!0)!!0) ) then do	
			if( (length(toCheck)) == 1) then do
				dellUserModify2 xe xa userGroupList (userID) sdlist vglist lvlist linklist fslist mplist name [] argsPrimary argsSecundary
			else 
				dellUserModify2 xe xa userGroupList (userID) sdlist vglist lvlist linklist fslist mplist name (tail(toCheck)) argsPrimary argsSecundary 	
		else 
			if((length(toCheck))==1) then do
				dellUserModify2 xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist mplist name [] argsPrimary argsSecundary
			else do
				dellUserModify2 xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist mplist name (tail(toCheck)) argsPrimary argsSecundary

deletePath2 xe dir xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name argsPrimary argsSecundary= do
	if (isNow 0 dir xe) == False then do
		putStrLn "User deleted, could not delete the path for the user because it does not exist"
		dellUserModify2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist mplist name userID argsPrimary argsSecundary
	else if isEmpty xe 0 dir == False then do
		putStrLn "The path for the user still exist, it contain files"
		dellUserModify2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist mplist name userID argsPrimary argsSecundary
	else if (isNow 0 dir xe) && isEmpty xe 0 dir then do
		dellUserModify2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist mplist name userID argsPrimary argsSecundary
		--body ((take ((getElem 0 dir xe)) xe )++(drop ((getElem 0 dir xe)+1) xe )) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		putStrLn "Error to delete path for the user"
		
deleteUserFromGroup2 xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck name argsPrimary argsSecundary = do
	if(null toCheck) then do
		if(null argsSecundary) then do
			createNewUser xe xa userID [] userGroupList [argsPrimary] name	0 sdlist vglist lvlist linklist fslist mplist
		else do 
			createNewUser xe xa userID [] userGroupList ([argsPrimary,"-G"]++argsSecundary) name	0 sdlist vglist lvlist linklist fslist mplist
	else do
		deleteUserFromGroup2 xe xa (userGroupList++[ [ (((toCheck!!0)!!0)),(((toCheck!!0)!!1)),(delName name (((toCheck!!0)!!2))),(delName name (((toCheck!!0)!!3))) ] ]) userID sdlist vglist lvlist linklist fslist mplist (tail(toCheck)) name argsPrimary argsSecundary







findUser xe xa userID userGroupList name j sdlist vglist lvlist linklist fslist mplist= do
	if ( j<=(length(userID)-1)) then do 
		if(((userID!!j)!!0)==name) then do
			printUserInformation xe xa [] userID name ((userID!!j)!!0) userGroupList 0 0 [] [] sdlist vglist lvlist linklist fslist mplist
		else do
			findUser xe xa userID userGroupList name (j+1) sdlist vglist lvlist linklist fslist mplist
	else do
		putStrLn $ "User not found"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist





deleteUser xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name i = do
	if (i > ((length(userID)-1))) then do
		putStrLn$"The user does not exist"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do 
		if( name == ((userID!!i)!!0)) then do -- the user exist
			 --deleteUser2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist mplist name userID
			deletePath xe ("home/"++((userID!!i)!!0)) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name
		else do
			deleteUser xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name (i+1)

deleteUser2 xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name toCheck= do
	if(null toCheck) then do
		deleteUserFromGroup xe xa [] (userID) sdlist vglist lvlist linklist fslist mplist userGroupList name -- Erase from the user groups
	else do
		if (name == ((toCheck!!0)!!0) ) then do	
			if( (length(toCheck)) == 1) then do
				deleteUser2 xe xa userGroupList (userID) sdlist vglist lvlist linklist fslist mplist name []
			else 
				deleteUser2 xe xa userGroupList (userID) sdlist vglist lvlist linklist fslist mplist name (tail(toCheck)) 	
		else 
			if((length(toCheck))==1) then do
				deleteUser2 xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist mplist name []
			else do
				deleteUser2 xe xa userGroupList (userID++[head(toCheck)]) sdlist vglist lvlist linklist fslist mplist name (tail(toCheck))

deleteUserFromGroup xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck name= do
	if(null toCheck) then do
		--putStrLn$(show(xe))
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		deleteUserFromGroup xe xa (userGroupList++[ [ (((toCheck!!0)!!0)),(((toCheck!!0)!!1)),(delName name (((toCheck!!0)!!2))),(delName name (((toCheck!!0)!!3))) ] ]) userID sdlist vglist lvlist linklist fslist mplist (tail(toCheck)) name
		
delName name toCheck = do
	if(null toCheck) then do
		[]
	else do
		if(name == toCheck!!0) then do
			delName name (tail(toCheck))
		else do 
			[head(toCheck)] ++ (delName name (tail(toCheck)))

deletePath xe dir xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name= do
	--putStrLn $ show (isNow 0 dir xe)
	--putStrLn $ show (isEmpty xe 0 dir)
	if (isNow 0 dir xe) == False then do
		putStrLn "User deleted, could not delete the path for the user because it does not exist"
		deleteUser2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist mplist name userID
	else if isEmpty xe 0 dir == False then do
		putStrLn "The path for the user still exist, it contain files"
		deleteUser2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist mplist name userID
	else if (isNow 0 dir xe) && isEmpty xe 0 dir then do
		deleteUser2 xe xa userGroupList [] sdlist vglist lvlist linklist fslist mplist name userID
		--body ((take ((getElem 0 dir xe)) xe )++(drop ((getElem 0 dir xe)+1) xe )) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		putStrLn "Error to delete path for the user"
		
deleteGroup xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name toCheck = do
	if(null toCheck) then do
		putStrLn "Borrado"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		if( (((toCheck!!0)!!0)!!0) == name) then do
			putStrLn"Found it"
			if( (length(toCheck)) == 1 ) then
				deleteGroup xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name []
			else do 
				deleteGroup xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name (tail(toCheck))
		else do 
			if( (length(toCheck)) == 1) then do 
				deleteGroup xe xa (userGroupList++[head(toCheck)]) userID sdlist vglist lvlist linklist fslist mplist name []
			else do
				deleteGroup xe xa (userGroupList++[head(toCheck)]) userID sdlist vglist lvlist linklist fslist mplist name (tail(toCheck))

--Username
--UID		path
--Associated primary group:
--Associated secondary groups:
printUserInformation xe xa userGroupList userID name id toCheck i j asPrimary asSecondary sdlist vglist lvlist linklist fslist mplist= do
	if(null toCheck) then do
		if (null asSecondary)then do
			putStrLn $ ("Username: "++name++" \n UID:"++id++"\t HomeDirectory:"++"/home/"++name++"\n\n User Primary Group: "++(show(findPrimaryFor name userGroupList [] 0)))
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		else do
			putStrLn $ ("Username: "++name++" \n UID:"++id++"\t HomeDirectory:"++"/home/"++name++"\n\n User Primary Group: "++(show(findPrimaryFor name userGroupList [] 0))++"\n\n User Secondary Groups:"++(show(findSecundaryFor name userGroupList [] 0)))
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist			
	else if ( i <= ( (length((toCheck!!0)!!2)))-1 ) then do		--There are still users as primary in the list of primary, otherwise we move to the users set as secondary
		if ( (((toCheck!!0)!!2)!!i) == name ) then do 								--The user is as Primary in this User Group
			printUserInformation xe xa userGroupList userID name id toCheck (i+1) j (((toCheck!!0)!!2)!!i) asSecondary sdlist vglist lvlist linklist fslist mplist
		else do																		--Let's check the next user set as primary
			printUserInformation xe xa userGroupList userID name id toCheck (i+1) j asPrimary asSecondary sdlist vglist lvlist linklist fslist mplist
	else if ( j <= (length((toCheck!!0)!!3))-1 ) then do
		if ( (((toCheck!!0)!!3)!!j)==name ) then do 								--The user is as secundary in this User Group
			printUserInformation xe xa userGroupList userID name id toCheck i (j+1) asPrimary (asSecondary++[(((toCheck!!0)!!3)!!j)]) sdlist vglist lvlist linklist fslist mplist
		else do																		--Let's check the next user set as secundary
			printUserInformation xe xa userGroupList userID name id toCheck i (j+1) asPrimary asSecondary sdlist vglist lvlist linklist fslist mplist
	else do	--This case means we go to the next group
		printUserInformation xe xa (userGroupList++ [head(toCheck)]) userID name id (tail(toCheck)) 0 0 asPrimary asSecondary sdlist vglist lvlist linklist fslist mplist
		
		
{------------------------------------------------Functions to logical volume--------------------------------------------------------}		

valueLogicalVolume xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck size volName vgName = do
	if(null toCheck ) then do
		putStrLn "The volume group doesn't exist"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do 
		if ( f(toCheck!!0) == vgName ) then do
			if( (si(toCheck!!0)) >= (getIntegerFrom size 0 0) ) then do
				--[(f(toCheck!!0)),(s(toCheck!!0)),(t(toCheck!!0)),((fo(toCheck!!0))+1),((s(toCheck!!0))++[volName]),(si(toCheck!!0)), ((l(toCheck!!0))-size) ] 
				body xe xa userGroupList userID sdlist (vglist ++ [( (f(toCheck!!0)),(s(toCheck!!0)),(t(toCheck!!0)),((fo(toCheck!!0))+1),((fi(toCheck!!0))++[volName]),(si(toCheck!!0)),  ( (l(toCheck!!0)) - (getIntegerFrom size 0 0))   )]  ++(tail(toCheck))) (lvlist++[volName,size,vgName]) linklist fslist mplist
			else do
				putStrLn "Not enough space"
				body xe xa userGroupList userID sdlist (vglist++toCheck) lvlist linklist fslist mplist
		else do
			valueLogicalVolume xe xa userGroupList userID sdlist (vglist++[head(toCheck)]) lvlist linklist fslist mplist (tail(toCheck)) size volName vgName

showInfoOfVG xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck= do
	if(null toCheck) then do
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do 
		putStrLn$"----- Volume group -----"
		putStrLn$"VG name \t\t\t"++(show((f(toCheck!!0))))
		putStrLn$"Cur LV \t\t\t"++(show((fo(toCheck!!0))))
		putStrLn$"Cur PV \t\t\t"++(show((t(toCheck!!0))))
		putStrLn$"VG size \t\t\t"++(show((si(toCheck!!0))))++" MiB"
		putStrLn$"Free size \t\t\t"++(show((l(toCheck!!0))))++" MiB"
		
		showInfoOfVG xe xa userGroupList userID sdlist (vglist++[head(toCheck)]) lvlist linklist fslist mplist (tail(toCheck))
		
		
		
showMoreInfoOfVG xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck= do
	if(null toCheck) then do 
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		putStrLn$"----- Volume group -----"
		putStrLn$"VG name \t\t\t"++(show((f(toCheck!!0))))
		putStrLn$"Cur LV \t\t\t"++(show((fo(toCheck!!0))))
		putStrLn$"Cur PV \t\t\t"++(show((t(toCheck!!0))))
		putStrLn$"VG size \t\t\t"++(show((l(toCheck!!0))))++" MiB"
		putStrLn$"Free size \t\t\t"++(show((si(toCheck!!0))))++" MiB"
		
		putStrLn$"-----Logical volumes -----"
		putStrLn $ show(fi(toCheck!!0))
		
		putStrLn$"-----Physical volumes -----"
		putStrLn $ (show ( s(toCheck!!0)))
		showMoreInfoOfVG xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist (tail(toCheck))
--		showinfoLogical xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck (getLogicalListFor lvlist (fi(toCheck!!0)) []) []

getLogicalListFor lvlist names ans = do
--	putStrLn$"ok"
  --  s <- ((lvlist!!0)!!0)
	if(null lvlist) then do
		ans
	else do 
		if( s == names ) then do 
			getLogicalListFor (tail(lvlist)) names (ans++[head(lvlist)])		 
		else do 
			getLogicalListFor (tail(lvlist)) names (ans)
			
getPhysicalListFor plist names ans = do
	putStrLn"ok"
	
showinfoLogical xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck logicalToPrint physicalToPrint = do
	if (null logicalToPrint ) then do
		showinfoPhysical xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck logicalToPrint physicalToPrint
	else do
		--putStrLn$(show(lvlist))
		putStrLn$"----- Logical volume -----"
		putStrLn$"LV path \t\t\t"
		putStrLn$"LV name \t\t\t"
		putStrLn$"VG name \t\t\t"
		putStrLn$"LV size \t\t\t"
		putStrLn$"      -----     "
		showinfoLogical xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck (tail(logicalToPrint)) physicalToPrint
		
showinfoPhysical xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck logicalToPrint physicalToPrint = do
	if(null physicalToPrint) then do
		showMoreInfoOfVG xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist (tail(toCheck))
	else do
		putStrLn$"----- Physical volume -----"
		putStrLn$"PV name \t\t\t"
		putStrLn$"Total size \t\t\t"
		putStrLn$"      -----     "
		showinfoPhysical xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck logicalToPrint (tail(physicalToPrint))
			 
			
{------------------------------------------------Functions to storage device--------------------------------------------------------}
createStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist newSD newSize i= do
	if( i <= ((length(sdlist))-1)) then do
		if( ((sdlist!!i)!!0) == (newSD) ) then do
			putStrLn $ "The storage device already exists"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist 
		else do		
			createStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist newSD newSize (i+1)
	else do  				--call to create the path		The first name indicates if LVM 	The second one indicates if it belongs to a volume group
		addFiles "d" xe newSD xa userGroupList userID (sdlist++[[newSD,newSize,"null"]]) vglist lvlist linklist fslist mplist
		--body xe xa userGroupList userID (sdlist++[[newSD,newSize,"null"]]) vglist lvlist linklist fslist mplist

listStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist i = do 
	if(i <= ((length(sdlist))-1) ) then do 
		if( ((sdlist!!i)!!2)=="LVM" ) then do
			putStrLn $ "Disk "++((sdlist!!i)!!0)++": "++((sdlist!!i)!!1)++" MiB Managed by: LVM"
			listStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist (i+1)
		else do
			putStrLn $ "Disk "++((sdlist!!i)!!0)++": "++((sdlist!!i)!!1)
			listStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist (i+1)
	else do	
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	
removeStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist nameToRemove toCheck= do
	if(null toCheck) then do
		--body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		rmFiles xe (nameToRemove) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		if( ((toCheck!!0)!!0) == nameToRemove) then do
			removeStorageDevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist nameToRemove (tail(toCheck))
		else
			removeStorageDevice xe xa userGroupList userID (sdlist++[head(toCheck)]) vglist lvlist linklist fslist mplist nameToRemove (tail(toCheck))

addLVMtodevice xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist name toCheck flag= do
	if (null toCheck) then do
		if(flag==True) then do
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist		
		else do
			putStrLn "Not such storage device"
			body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		if( ((toCheck!!0)!!0) == name) then do
			addLVMtodevice xe xa userGroupList (userID) (sdlist++[[ ((toCheck!!0)!!0), ((toCheck!!0)!!1), "LVM" ]]) vglist lvlist linklist fslist mplist name (tail(toCheck)) True
		else do
			addLVMtodevice xe xa userGroupList (userID) (sdlist++[head(toCheck)]) vglist lvlist linklist fslist mplist name (tail(toCheck)) flag

removeSD xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist nameToRemove toCheck= do
	if(null toCheck) then do
		True
	else do
		if( ((toCheck!!0)!!0) == nameToRemove) then do
			removeSD xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist nameToRemove (tail(toCheck))
		else
			removeSD xe xa userGroupList userID (sdlist++[head(toCheck)]) vglist lvlist linklist fslist mplist nameToRemove (tail(toCheck))

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
			getIntegerFrom ((sdlist!!0)!!1) 0 0
		else do
			getSize (tail(sdlist)) name

			
{------------------------------------------------Functions to manage Files--------------------------------------------------------}
echoFile xe xa doc dir userGroupList userID sdlist vglist lvlist linklist fslist mplist= do
	putStrLn $ show ("/"++ dir)
	putStrLn $ show (isNow 0 ("/"++ dir) xe)
	putStrLn $ show (l(xe !! ((getElem 0 dir xe)))  == "-")
	if (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) ) == "-" then do
		--putStrLn $ show((getElem 0 dir xe))
		let tuple = (f(xe !! (getElem 0 dir xe)),s(xe !! (getElem 0 dir xe)),t(xe !! (getElem 0 dir xe)),fo(xe !! (getElem 0 dir xe)),fi(xe !! (getElem 0 dir xe)),doc,l(xe !! (getElem 0 dir xe)))
		body ((fhalf 0 xe (getElem 0 dir xe) []) ++ [tuple] ++(shalf ((getElem 0 dir xe)+1) xe (getElem 0 dir xe) []))  xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		putStrLn "Fallo"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist	
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
catFile xe xa dir sdlist vglist lvlist linklist fslist mplist=do
	if (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "-") then do
		putStrLn $ show (si(xe !! (getElem 0 dir xe) ))
	else
		putStrLn "No existe"
	
{------------------------------------------------Functions to manage the file system-----------------------------------------------------}	
createFileSystem xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist exttype path toCheck= do
	if (null toCheck) then do
		body xe xa userGroupList userID sdlist vglist lvlist linklist (fslist++[[exttype,path,"null"]]) mplist
	else do
		if ((toCheck!!0)!!0== path) then do
			putStrLn"It already exist"
		else 	
			createFileSystem xe xa userGroupList userID sdlist vglist lvlist linklist (fslist++[head(toCheck)]) mplist exttype path (tail(toCheck))

			


mountPoint xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist path mountPointpath toCheck = do
	if(null toCheck) then do 
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		if (((toCheck!!0)!!1)==path) then do
			--putStrLn$"ok"
			mountPoint xe xa userGroupList userID sdlist vglist lvlist linklist (fslist++[[(((toCheck!!0)!!0)),(((toCheck!!0)!!1)),mountPointpath]]) mplist path mountPointpath (tail(toCheck))
		else do 
			--(putStrLn$("ok"))
			mountPoint xe xa userGroupList userID sdlist vglist lvlist linklist (fslist++[head(toCheck)]) mplist path mountPointpath (tail(toCheck))



showFileSystemOut xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist toCheck= do
	if(null toCheck) then do
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do 
		putStrLn$( ((toCheck!!0)!!1) ++ "\t\t\t" ++ ((toCheck!!0)!!0) ++ "\t\t\t" ++((toCheck!!0)!!2))
		showFileSystemOut xe xa userGroupList userID sdlist vglist lvlist linklist (fslist++[head(toCheck)]) mplist (tail(toCheck))

{------------------------------------------------Functions to manage the Path-----------------------------------------------------}

rmFiles xe dir xa userGroupList userID sdlist vglist lvlist linklist fslist mplist= do
	putStrLn $ show (isNow 0 dir xe)
	putStrLn $ show (isEmpty xe 0 dir)
	if (isNow 0 dir xe) == False then do
		putStrLn "El archivo no existe"
	else if isEmpty xe 0 dir == False then do
		putStrLn "La carpeta no esta vacia"
		
	else if (isNow 0 dir xe) && isEmpty xe 0 dir then do
		body ((take ((getElem 0 dir xe)) xe )++(drop ((getElem 0 dir xe)+1) xe )) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
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
	


addFiles mode xe add xa userGroupList userID sdlist vglist lvlist linklist fslist mplist = do
	now <- getCurrentTime
	time <- getCurrentTimeZone
	if isNow 0 add xe then do
		--putStrLn $"Error creating the path: '"++(show(add))++"' already exist"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if ("/" `isInfixOf` add )then do
		splitAdd mode mode 0 (splitOn "/" add) xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if (f (head xe )) == "/" then do
		body (xe ++ [(add ,f(head xe) ++ add,getDate now,"time","root","",mode)]) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do 
		body (xe ++ [(add, s(head xe)++"/" ++ add,getDate now,"Hora","root","",mode)]) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist

addFiles2 mode modeT cont dir xe add xa userGroupList userID sdlist vglist lvlist linklist fslist mplist= do
	now <- getCurrentTime
	if (f (head xe )) == "/" then do
		--putStrLn "d"
		splitAdd mode modeT (cont + 1) dir (xe ++ [(last(splitOn "/" add) ,f(head xe) ++ add,getDate now,"time","root","",mode)]) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		splitAdd mode modeT (cont + 1) dir (xe ++ [(last(splitOn "/" add) ,f(head xe) ++"/"++ add,getDate now,"time","root","",mode)]) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		
splitAdd mode modeT cont dir xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist= do
	if (cont == 0) then do
		--putStrLn "s"
		addFiles2 "d" modeT cont dir xe (dir !! 0) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if (cont == (length dir)-1) && modeT == "-" then do
		addFiles2 modeT modeT cont dir xe (f(last xe)++"/"++ (dir !! cont)) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if (cont < length dir) && (cont /= 0) then do
		addFiles2 "d" modeT cont dir xe (f(last xe)++"/"++ (dir !! cont)) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist	
	else
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist


listFiles dir cont  xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist= do
	
	if (cont < length xe) then do
		if (f(head xe ) `isInfixOf` s(xe !! cont)) && (f( xe !! 2) /= s(xe !! (cont))) && (dir =="none")  then do
			putStrLn $ f(xe !! cont)
			listFiles dir (cont + 1) xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		else if (dir `isInfixOf` s(xe !! cont)) && (f( xe !! 2) /= s(xe !! (cont)))  then do
			putStrLn $ f(xe !! cont)
			listFiles dir (cont + 1) xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		else do
			listFiles dir (cont + 1 ) xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do 
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist

moveDirectory dir xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist= do
	if dir == ".." then do
		putStrLn "retroceder"
	else if dir == "/" then do
		body ((xe!!1):tail xe) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist	
	else if (s(head xe) == "/") && (isNow 0 dir xe) && (l(xe !! (getElem 0 dir xe)) == "d") then do
		body ((xe !! getElem 0 dir xe):tail xe) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if  (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "d") then do
		body ((xe !! ((getElem 0 dir xe))):tail xe) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if  (isNow 0 (dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "d") then do
		body ((xe !! ((getElem 0 dir xe))):tail xe) xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else if  (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "-") then do
		putStrLn "Archivo"
	else do 
		putStrLn "No existe"
		body xe xa userGroupList userID sdlist  vglist lvlist linklist fslist mplist

	
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
createdDev size dir xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist= do 
	if (isNow2 0 dir xe) then do
		putStrLn "SIPP" 
		body xe ((dir,size,dir):xa) userGroupList userID sdlist vglist lvlist linklist fslist mplist
	else do
		putStrLn "El directorio no existe"
		body xe xa userGroupList userID sdlist vglist lvlist linklist fslist mplist
		


		

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