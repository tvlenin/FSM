main:: IO()

main = do 
	body [["/"]] [	[["Group1"],["1000"],["root"],["secundario"]]	]
--	body [["/"]] [[[""],[""],[""],[""]]]
	
	
{-	 
	This is the main loop, where the input line is taken, according to the correct command
-}		
body xe userGroupList = do 

	putStr "$"
	cmd <- getLine


	{-	Command : #groupadd <nombre del grupo> 
		Here is possible to add a new user group
	-}	
	if  head(words(cmd)) == "groupadd" && length(words(cmd))==2 then do			--The sinstaxis must be correct
		let id = (read ((((last(userGroupList))!!1)!!0)) :: Integer)+1 			--Increase the counter
		addUserGroup xe userGroupList [[(words(cmd)!!1)],[ show (id) ],[],[	]]	--Call the function to add the new group
	
	{- 	Command : # useradd -g primaryGroup [-G secondaryGroup1 secondaryGroup2] userName
		An user must have an primary group associated, the secondary ones are optional
	-}
	else if head(words(cmd)) == "useradd" && (words(cmd)!!1) =="-g" && length(words(cmd))>=4 then do
		--[GPrimario, Gsecundario, nombreUsuario ]
		--agregar a los grupos
		--crear el path correcto
				
	else if head(words(cmd)) == "pvcreate" then do
		line <- getLine
		addFiles xe [line] userGroupList	
	
	else if head(words(cmd)) == "vgcreate" then do
		line <- getLine
		findFiles xe line userGroupList
	
	else do
		putStrLn $ "digite el comando correcto"
	body xe userGroupList
	

addFiles xe add userGroupList = do
	putStrLn  "hola"
	let tmp = xe ++ [add]
	putStrLn $ show tmp
	body tmp userGroupList   
	
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
	else do
		putStrLn "no existe"
		
	body xe userGroupList
	
	    

