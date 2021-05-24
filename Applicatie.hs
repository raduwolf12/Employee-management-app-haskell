import System.IO
import System.Console.ANSI
import System.Directory
import Data.Char 
import Data.List
import Data.List.Split
import Control.Exception
import System.IO.Error hiding (catch)
import ReadModule
import Angajat
import HSFSDF


-- functie care decide care dintre operatiile din aplicatie sa fie efectuate
execopt::String->IO() 
execopt opt | (opt=="1")=do{
                            clearScreen;
                            viewAngajati;
                            putStrLn "Press enter to exit...";
                            wait<-getLine;
                            clearScreen;
                            main
                           }
execopt opt | (opt=="2")=do
                            clearScreen;
                            setCursorPosition 5 30;
                            putStr "Introduceti codul unui angajat :";
                            hFlush stdout;
                            number <- getLine;
                            shwowData number
                            putStrLn "Press enter to exit...";
                            wait<-getLine;
                            clearScreen;
                            main
execopt opt | (opt=="3")=do
                            clearScreen;
                            setCursorPosition 5 30;
                            putStrLn "Introduceti datele angajatului:";
                            createAngajat
                            putStrLn "Press enter to exit...";
                            wait<-getLine;
                            clearScreen;
                            main
execopt opt | (opt=="4")=do
                            clearScreen;
                            setCursorPosition 5 30;
                            viewAngajati
                            putStrLn "Stergeti angajatul de pe linia :";
                            id<-getLine
                            deleteAngajat id
                            putStrLn "Press enter to exit...";
                            wait<-getLine;
                            clearScreen;
                            main                                  

-- metoda prin care vad angajatii din fisieru; cu angajati
viewAngajati :: IO() 
viewAngajati = do
   let fileName = "angajati.txt"

   fileExist <- doesFileExist fileName

   if not fileExist
   then writeFile fileName ""
   else return ()
   setCursorPosition 5 30;
   putStrLn "Angajatii firmei sunt:";
   ReadModule.createTabelAngajati "angajati.txt"

-- metoda prin care vad datele despre un angajat cu un anumit uid
shwowData::String->IO()
shwowData number  =do
 setCursorPosition 9 30;
 if all isSpace number
   then putStrLn "err"
 else serchAngajat number 
--  metoda de cautare a angajatuui dupa un uid
serchAngajat::String->IO()
serchAngajat val =do
  let angajatStudiiFile = val++"/infoStudii.txt"
  let angajatContactFile = val++"/infoContact.txt"
  let angajatExperientaFile = val++"/infoExperienta.txt"
  let angajatVenituriFile = val++"/infoVenituri.txt"

  fileExist <- doesFileExist angajatContactFile

  if not fileExist
  then do
    setCursorPosition 7 30 
    putStrLn "Nu exista angajatul cu cnp-ul specificat!"
    setCursorPosition 8 30
    putStrLn "Apasati orice tasta pentru a iesi!"
    opt<- getLine
    clearScreen
    main
  else do 
    showOptions
    op<-getLine
    viewAngajatInfo op val
    putStrLn "Doriti sa mai vedeti alte informatii?"
    putStrLn "D/N"
    ops<-getLine
    if ops=="D"
    then do 
      clearScreen
      setCursorPosition 5 30
      let string="Introduceti codul unui angajat :"++val
      putStrLn string
      serchAngajat val
    else do
      clearScreen
      main



-- metoda prin care afisez anumite informatii despre angajat
viewAngajatInfo::String->String->IO()
viewAngajatInfo opt val | (opt=="1")=do
  let angajatStudiiFile = val++"/infoStudii.txt"

  fileExist <- doesFileExist angajatStudiiFile
  if not fileExist
  then putStrLn "Error fisierul infoStudii.txt nu exista"
  else do 
    -- contents <- readFile angajatStudiiFile
    -- putStrLn contents
    ReadModule.createTabelStudii angajatStudiiFile
viewAngajatInfo opt val | (opt=="2")=do
  let file = val++"/infoExperienta.txt"

  fileExist <- doesFileExist file
  if not fileExist
  then putStrLn "Error fisierul infoExperienta.txt nu exista"
  else do 
    -- contents <- readFile file
    -- putStrLn contents
    ReadModule.createTabelExperiente file
viewAngajatInfo opt val | (opt=="3")=do
  let file = val++"/infoContact.txt"

  fileExist <- doesFileExist file
  if not fileExist
  then putStrLn "Error fisierul infoContact.txt nu exista"
  else do 
    ReadModule.createTabelContact file
viewAngajatInfo opt val | (opt=="4")=do
  let file = val++"/infoVenituri.txt"

  fileExist <- doesFileExist file
  if not fileExist
  then putStrLn "Error fisierul infoVenituri.txt nu exista"
  else do 
    -- contents <- readFile file
    -- putStrLn contents
    ReadModule.createTabelVenit file
viewAngajatInfo opt val | (opt=="5")=do
  let file = val++"/infoFile.txt"

  fileExist <- doesFileExist file
  if not fileExist
  then putStrLn "Error fisierul infoFile.txt nu exista"
  else do 
    ReadModule.createTabelInfo file

-- metoda prin care afisez optiunile pe care le are utilizatorul legat de afisarea de informatii 
showOptions::IO()
showOptions =do
  setCursorPosition 7 30
  putStrLn "|---------------------------------------|"
  setCursorPosition 8 30
  putStrLn "|  1-Afisare studiile angajatului       |"
  setCursorPosition 9 30
  putStrLn "|  2-Afiseaza experienta profesionala   |"
  setCursorPosition 10 30
  putStrLn "|  3-Afiseaza datele de contact         |"
  setCursorPosition 11 30
  putStrLn "|  4-Afiseaza veniturile angajatului    |"
  setCursorPosition 12 30
  putStrLn "|  5-Afiseaza informatii personale      |"
  setCursorPosition 13 30
  putStrLn "|  6-Iesire                             |"
  setCursorPosition 14 30
  putStrLn "|---------------------------------------|"
-- metoda de creeare a unui angajat
createAngajat::IO()
createAngajat = do

    let angajatiFile = "angajati.txt"

    fileExist <- doesFileExist angajatiFile

    if not fileExist
    then writeFile angajatiFile " "
    else return ()

    putStr "Nume:"    
    hFlush stdout;
    nume <- getLine
    appendFile angajatiFile "\n" ;
    appendFile angajatiFile nume ;
    appendFile angajatiFile "," ;
    
    putStr  "Prenume:"
    hFlush stdout;
    prenume <- getLine
    appendFile angajatiFile prenume ;
    appendFile angajatiFile "," ;

    putStr "Cnp:"
    hFlush stdout;
    cnp <- getLine

    appendFile angajatiFile cnp ;
    appendFile angajatiFile "," ;

    createDirectory cnp
  -- info personale
    let angajatInfoFile = cnp++"/infoFile.txt"
    writeFile angajatInfoFile ","

    appendFile angajatInfoFile cnp ;
    appendFile angajatInfoFile "," ;
    
    putStr "Seria:"
    hFlush stdout;
    seria <- getLine
    appendFile angajatInfoFile seria ;
    appendFile angajatInfoFile "," ;

    putStr "Data nasterii:"
    hFlush stdout;
    data_nastere <- getLine

    appendFile angajatInfoFile data_nastere ;
    appendFile angajatInfoFile "," ;

    putStr "IBAN:"
    hFlush stdout;
    iban <- getLine
    appendFile angajatInfoFile iban ;
    appendFile angajatInfoFile "," ;

  -- info contact
    let angajatContactFile = cnp++"/infoContact.txt"
    writeFile angajatContactFile ","

    putStr "Telefon:"
    hFlush stdout;
    telefon <- getLine
    appendFile angajatContactFile telefon ;
    appendFile angajatContactFile "," ;

    putStr "Fax:"
    hFlush stdout;
    fax <- getLine
    appendFile angajatContactFile fax ;
    appendFile angajatContactFile "," ;

    putStr "Email:"
    hFlush stdout;
    email <- getLine
    appendFile angajatContactFile email ;
    appendFile angajatContactFile "," ;

    putStr "Salariu:"
    hFlush stdout;
    salariu <- getLine
    let angajatVenitFile = cnp++"/infoVenituri.txt"
    writeFile angajatVenitFile " "
    appendFile angajatVenitFile salariu ;
    appendFile angajatVenitFile "," ;

    putStrLn "Adauga studii angajat:"
    putStrLn "Doriti sa adaugati studii?"
    putStrLn "D/N"
    op<-getLine
    addMultipleStudii op cnp 

    
    putStrLn "Angajatul a fost creat"
-- metoda adaugarea a unor studii
addStudii::String->IO()
addStudii cnp= do
  let angajatStudiiFile = cnp++"/infoStudii.txt"
  
  fileExist <- doesFileExist angajatStudiiFile

  if not fileExist
  then writeFile angajatStudiiFile " "
  else return ()


  putStr "Unitate de invatamant:"
  hFlush stdout;
  invatamant <- getLine
  appendFile angajatStudiiFile invatamant ;
  appendFile angajatStudiiFile " " ; 

  putStr "profil:"
  hFlush stdout;
  profil <- getLine
  appendFile angajatStudiiFile profil ;
  appendFile angajatStudiiFile " " ;  
  
  putStr "Nume institutie:"
  hFlush stdout;
  numeInst <- getLine
  appendFile angajatStudiiFile numeInst ;
  appendFile angajatStudiiFile " " ;  

  putStr "Data inceperi studiilor:"
  hFlush stdout;
  inceput <- getLine
  appendFile angajatStudiiFile inceput ;
  appendFile angajatStudiiFile " " ;  

  putStr "Data finalizarii studiilor:"
  hFlush stdout;
  final <- getLine
  appendFile angajatStudiiFile final ;
  appendFile angajatStudiiFile " " ;
  appendFile angajatStudiiFile "\n" ;   
-- metoa de agaugare a experientei unui angajat
addExperienta::String->IO()
addExperienta cnp= do
  let angajatExperientaFile = cnp++"/infoExperienta.txt"
  
  fileExist <- doesFileExist angajatExperientaFile

  if not fileExist
  then writeFile angajatExperientaFile " "
  else return ()

  putStr "Tip de experienta(Practica/Intership/Job):"
  hFlush stdout;
  tip <- getLine
  appendFile angajatExperientaFile tip ;
  appendFile angajatExperientaFile " " ;
 
  putStr "Numele firmei:"
  hFlush stdout;
  firma <- getLine
  appendFile angajatExperientaFile firma ;
  appendFile angajatExperientaFile " " ;
  
  putStr "Perioada:"
  hFlush stdout;
  perioada <- getLine
  appendFile angajatExperientaFile perioada ;
  appendFile angajatExperientaFile " " ;
  appendFile angajatExperientaFile "\n" ;

addMultipleExperienta::String->String->IO() 
addMultipleExperienta opt cnp | (opt=="D")=do
  addExperienta cnp
  putStrLn "Doriti sa mai adaugati experienta profesionala?"
  putStrLn "D/N"
  op<-getLine
  addMultipleExperienta op cnp
addMultipleExperienta opt cnp | (opt=="N")=do
  putStrLn "Finalizare experienta"
  clearScreen;
  main
 


addMultipleStudii::String->String->IO() 
addMultipleStudii opt cnp | (opt=="D")=do
  addStudii cnp
  putStrLn "Doriti sa mai adaugati studii?"
  putStrLn "D/N"
  op<-getLine
  addMultipleStudii op cnp
addMultipleStudii opt cnp | (opt=="N")=do
  putStrLn "Finalizare studii"
  putStrLn "Doriti sa adaugati experienta?"
  putStrLn "D/N"
  op<-getLine
  addMultipleExperienta op cnp

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- metoda de stergere a unui angajat
deleteAngajat :: String -> IO ()
deleteAngajat id = do 
    let fileName = "angajati.txt" 
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let toDelete = read id
    let angajati = lines contents
    let newAngajatStr = angajati !! (toDelete - 1)
    putStrLn newAngajatStr

    let split = splitOn "," newAngajatStr
    let cnp = split!!2
    putStrLn cnp
    let newAngajati = delete (angajati !! (toDelete - 1)) angajati
    hPutStr tempHandle (unlines newAngajati) 
    hClose handle
    hClose tempHandle

    removePathForcibly cnp
    deleteFileAngajat tempName fileName

    
    -- removeIfExists fileName
    -- renameFile tempName fileName
    

    putStrLn "Angajat sters."

-- metoda de stergere a fisierelor unui angajat  
deleteFileAngajat :: String ->String -> IO ()
deleteFileAngajat tempName fileName = do
  removeIfExists fileName
  renameFile tempName fileName
  putStrLn "Angajat sters." 


main::IO()
main = do
{
    setTitle "EVIDENTA ANGAJATILOR UNEI FIRME";
    setCursorPosition 5 30;
    putStrLn "Optiuni program...";
    setCursorPosition 6 30;
    putStrLn "1-Vezi lista angajati !";
    setCursorPosition 7 30;
    putStrLn "2-Selecteaza un angajat !";
    setCursorPosition 8 30;
    putStrLn "3-Adauga un angajat !";
    setCursorPosition 9 30;
    putStrLn "4-Sterge un angajat !";
    setCursorPosition 10 30;
    putStrLn "5-Exit";
    setCursorPosition 11 30;
    putStr "Optiunea Dvs.: ";
    hFlush stdout;
    setCursorPosition 11 30;
    opt<-getLine;
    execopt(opt);
    clearScreen;
    main;
}