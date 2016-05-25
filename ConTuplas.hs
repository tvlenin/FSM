import Data.List
import Data.List.Split
import Data.Char
import Data.Time
main:: IO()

main = do 
	now <- getCurrentTime
	putStrLn "Inicio del File System"
	body [("/","/",getDate now,"15:32","root:root"),("/","/",getDate now,"15:32","root:root")] [("","","")]
body xe xa  = do 
	putStrLn $ show xe
	op <- getLine
	if op == "cdv" then do
		putStrLn $ show xa
		size <- getLine
		dir <- getLine
		createdDev size dir xe xa
	else if op == "in" then do
		putStrLn "ingrese el nombre de la carpeta"
		line <- getLine
		addFiles xe line xa
		
	else if op == "ls" then do
		putStrLn "buscando"
		listFiles 0 xe xa
	else if op == "mv" then do
		putStrLn "Digite la carpte"
		line <- getLine
		moveDirectory line xe xa
	else do
		putStrLn "Fin"
	

{------------------------------------------------Functions to manage the Path-----------------------------------------------------}

addFiles xe add xa= do
	now <- getCurrentTime
	time <- getCurrentTimeZone
	if ("/" `isInfixOf` add )then do
		splitAdd 0 (splitOn "/" add) xe xa
	else if (f (head xe )) == "/" then do
		body (xe ++ [(add ,f(head xe) ++ add,getDate now,"time","root")]) xa
	else do 
		body (xe ++ [(add, s(head xe)++"/" ++ add,getDate now,"Hora","root")]) xa	
--Auxiliar addFiles to create directories recursive addFiles2 and splitAdd work together
addFiles2 cont dir xe add xa= do
	now <- getCurrentTime
	if (f (head xe )) == "/" then do
		--putStrLn "d"
		splitAdd (cont + 1) dir (xe ++ [(last(splitOn "/" add) ,f(head xe) ++ add,getDate now,"time","root")]) xa
	else do
		putStrLn "NOKO" 
		--body (xe ++ [(add, s(head xe)++"/" ++ add,getDate now,"Hora","root")]) xa
		
splitAdd cont dir xe xa =do
	if (cont == 0) then do
		putStrLn "s"
		addFiles2 cont dir xe (dir !! 0) xa
	else if (cont < length dir) && (cont /= 0) then do
		putStrLn "s"
		addFiles2 cont dir xe (f(last xe)++"/"++ (dir !! cont)) xa
		
	else
		body xe xa

--Function to list the directories
listFiles cont  xe xa = do
	
	if (cont < length xe) then do
		if (f(head xe ) `isInfixOf` s(xe !! cont)) && (f( xe !! 2) /= s(xe !! (cont)))  then do
			putStrLn $ f(xe !! cont)
			listFiles (cont + 1) xe xa
		else do
			listFiles (cont + 1 ) xe xa
	else do 
		body xe xa

moveDirectory dir xe xa = do
	if dir == ".." then do
		putStrLn "retroceder"
	else if dir == "/" then do
		body ((xe!!1):tail xe) xa		
	else if (s(head xe) == "/") && (isNow 0 dir xe) then do
		body ((xe !! getElem 0 dir xe):tail xe) xa
	else if  (isNow 0 ("/"++ dir) xe) then do
		
		body ((xe !! ((getElem 0 dir xe)-1)):tail xe) xa
	else do 
		putStrLn "No existe"
		body xe xa

	
getElem cont dir xe = do
	if (cont < length xe )then do
		if (s (head xe ) ++ dir) == s(xe !! cont) then do 
			cont
		else do
			getElem (cont +1) dir xe 
	else do
		cont
		
	
isNow cont dir xe = do
	if (cont < length xe )then do
		if (s (head xe ) ++ dir) == s(xe !! cont) then do 
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
createdDev size dir xe xa = do 
	if (isNow2 0 dir xe) then do
		putStrLn "SIPP" 
		body xe ((dir,size,dir):xa)
	else do
		putStrLn "El directorio no existe"
		body xe xa
		


		

isNow2 cont dir xe = do	
	if (cont < length xe )then do
		if (dir) == s(xe !! cont) then do 
			True
		else do
			isNow2 (cont +1) dir xe 
	else 
		False




--End Device Storage





f (a, _, _, _, _) = a
s (_, a, _, _, _) = a
t (_, _, a, _, _) = a
fd (a, _, _) = a
sd (_, a, _) = a
td (_, _, a) = a