main:: IO()

main = do 
	putStrLn "Inicio del File System"
	body [["/"]]

body xe = do 
	putStrLn "Las carpetas actuales son"
	putStrLn $ show xe
	putStrLn "Digite in para ingresar o find para buscar"
	op <- getLine
	
	if op == "in" then do
		putStrLn "ingrese el nombre de la carpeta"
		line <- getLine
		addFiles xe [line]	
		
	
	else if op == "find" then do
		putStrLn "Digite el nombre de la carpeta a buscar"
		line <- getLine
		findFiles xe line
	
	else do
		putStrLn $ "digite el comando correcto"
		body xe
	
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
	
	    

