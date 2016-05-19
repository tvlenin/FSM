main:: IO()

main = do 
	body [
			[
				["nombre"],["1000"],["root"],["secundario1"]]
			]
		]
	--body[["/"]]
		
body xe = do 
	putStr "$"
	cmd <- getLine
	
	if  head(words(cmd)) == "groupadd" && length(words(cmd))==2 then do
		line <- getLine
		putStrLn "a"
	
	else if head(words(cmd)) == "pvcreate" then do
		line <- getLine
		addFiles xe [line]	
	
	else if head(words(cmd)) == "vgcreate" then do
		line <- getLine
		findFiles xe line
	
	else do
		putStrLn $ "digite el comando correcto"
	body xe
	
	
--addUserGroup xe = do
	

addFiles xe add  = do
	putStrLn  "hola"
	let tmp = xe ++ [add]
	putStrLn $ show tmp
	body tmp   

findFiles xe find  = do
	if [find] `elem` xe then do
		putStrLn "Si existe"
	else do
		putStrLn "no existe"
		
	body xe
	
	    

