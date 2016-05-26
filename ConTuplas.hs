import Data.List
import Data.List.Split
import Data.Char
import Data.Time
main:: IO()

main = do 
	now <- getCurrentTime
	putStrLn "Inicio del File System"
	body [("/","/",getDate now,"15:32","root:root","","d"),("/","/",getDate now,"15:32","root:root","","d")] [("","","")]
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
		addFiles "d" xe line xa
	else if op == "rm" then do
		putStrLn "ingrese el nombre de la carpeta"
		line <- getLine
		rmFiles xe line xa
		
		
	else if op == "touch" then do
		putStrLn "ingrese el nombre del archivo"
		line <- getLine
		addFiles "-" xe line xa
	else if op == "echo" then do
		putStrLn "ingrese el archivo a modificar"
		dir <- getLine
		putStrLn "ingrese el contenido del archivo"
		line <- getLine
		echoFile xe xa line dir
	else if op == "cat" then do
		putStrLn "ingrese el archivo a ver"
		dir <- getLine
		catFile xe xa dir
		
	else if op == "ls" then do
		putStrLn "buscando"
		listFiles 0 xe xa
	else if op == "mv" then do
		putStrLn "Digite la carpte"
		line <- getLine
		moveDirectory line xe xa
	else do
		putStrLn "Fin"
	








{------------------------------------------------Functions to manage Files--------------------------------------------------------}
echoFile xe xa doc dir = do
	if (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "-") then do
		--putStrLn $ show((getElem 0 dir xe))
		let tuple = (f(xe !! (getElem 0 dir xe)),s(xe !! (getElem 0 dir xe)),t(xe !! (getElem 0 dir xe)),fo(xe !! (getElem 0 dir xe)),fi(xe !! (getElem 0 dir xe)),doc,l(xe !! (getElem 0 dir xe)))
		body ((fhalf 0 xe (getElem 0 dir xe) []) ++ [tuple] ++(shalf ((getElem 0 dir xe)+1) xe (getElem 0 dir xe) []))  xa
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
catFile xe xa dir =do
	if (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "-") then do
		putStrLn $ show (si(xe !! (getElem 0 dir xe) ))
	else
		putStrLn "No existe"
	
{------------------------------------------------Functions to manage the Path-----------------------------------------------------}

rmFiles xe dir xa = do
	putStrLn $ show (isNow 0 dir xe)
	putStrLn $ show (isEmpty xe 0 dir)
	if (isNow 0 dir xe) == False then do
		putStrLn "El archivo no existe"
	else if isEmpty xe 0 dir == False then do
		putStrLn "La carpeta no esta vacia"
		
	else if (isNow 0 dir xe) && isEmpty xe 0 dir then do
		body ((take ((getElem 0 dir xe)) xe )++(drop ((getElem 0 dir xe)+1) xe )) xa
	else do
		putStrLn "No Se"

	

isEmpty xe cont dir = do
	
	if cont < length xe then do
		if (dir `isInfixOf` s(xe !! cont)) && (("/"++dir) /= s(xe !! cont)) then do
			False
		else do
			isEmpty xe (cont+1) dir
	else
		True
	


addFiles mode xe add xa= do
	now <- getCurrentTime
	time <- getCurrentTimeZone
	if isNow 0 add xe then do
		putStrLn "Already exists"
		body xe xa
	else if ("/" `isInfixOf` add )then do
		splitAdd mode mode 0 (splitOn "/" add) xe xa
	else if (f (head xe )) == "/" then do
		body (xe ++ [(add ,f(head xe) ++ add,getDate now,"time","root","",mode)]) xa
	else do 
		body (xe ++ [(add, s(head xe)++"/" ++ add,getDate now,"Hora","root","",mode)]) xa	

addFiles2 mode modeT cont dir xe add xa= do
	now <- getCurrentTime
	if (f (head xe )) == "/" then do
		--putStrLn "d"
		splitAdd mode modeT (cont + 1) dir (xe ++ [(last(splitOn "/" add) ,f(head xe) ++ add,getDate now,"time","root","",mode)]) xa
	else do
		splitAdd mode modeT (cont + 1) dir (xe ++ [(last(splitOn "/" add) ,f(head xe) ++"/"++ add,getDate now,"time","root","",mode)]) xa
		
splitAdd mode modeT cont dir xe xa =do
	if (cont == 0) then do
		putStrLn "s"
		addFiles2 "d" modeT cont dir xe (dir !! 0) xa
	else if (cont == (length dir)-1) && modeT == "-" then do
		putStrLn "Entra"
		addFiles2 modeT modeT cont dir xe (f(last xe)++"/"++ (dir !! cont)) xa
	else if (cont < length dir) && (cont /= 0) then do
		putStrLn "5"
		addFiles2 "d" modeT cont dir xe (f(last xe)++"/"++ (dir !! cont)) xa
		
	else
		body xe xa


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
	else if (s(head xe) == "/") && (isNow 0 dir xe) && (l(xe !! (getElem 0 dir xe)) == "d") then do
		body ((xe !! getElem 0 dir xe):tail xe) xa
	else if  (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "d") then do
		body ((xe !! ((getElem 0 dir xe))):tail xe) xa
	else if  (isNow 0 ("/"++ dir) xe) && (l(xe !! ((getElem 0 dir xe))) == "-") then do
		putStrLn "Archivo"
	else do 
		putStrLn "No existe"
		body xe xa

	
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