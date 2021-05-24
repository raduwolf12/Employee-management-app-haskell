module ReadModule where
    import System.IO
    import Data.List.Split

    readDataFrom fileHandle = 
            do 
                isFileEnd <- hIsEOF fileHandle
                if isFileEnd 
                    then
                        
                        return ("")
                    else
                        do
                            info <- hGetLine  fileHandle
                            putStrLn info
                            putStrLn "|---------------------------------------|"
                            readDataFrom fileHandle


    readDataFromA fileHandle = 
        do 
            isFileEnd <- hIsEOF fileHandle
            if isFileEnd 
                then
                    
                    return ("")
                else
                    do
                        info <- hGetLine  fileHandle
                        -- putStrLn info
                        let split = splitOn "," info
                        let nume = split!!0
                        let prenume = split!!1
                        let uid = split!!2
                        let row = "|"++nume ++ "|"++ prenume ++ "|"++uid++ "|"
                        putStrLn row
                        putStrLn "|---------------------------------------|"
                        readDataFromA fileHandle
    readDataFromInfo fileHandle = 
        do 
            isFileEnd <- hIsEOF fileHandle
            if isFileEnd 
                then
                    
                    return ("")
                else
                    do
                        info <- hGetLine  fileHandle
                        -- putStrLn info
                        let split = splitOn "," info
                        let cnp = split!!0
                        let serie = split!!1
                        let nas = split!!2
                        let iban = split!!3
                        let row = "|"++cnp ++ "|"++ serie ++ "|"++nas++ "|"++iban++ "|"
                        putStrLn row
                        putStrLn "|--------------------------------------------------|"
                        readDataFromA fileHandle

    
    readDataFromStudii fileHandle = 
        do 
            isFileEnd <- hIsEOF fileHandle
            if isFileEnd 
                then
                    
                    return ("")
                else
                    do
                        info <- hGetLine  fileHandle
                        -- putStrLn info
                        let split = splitOn "," info
                        let ui = split!!0
                        let profil = split!!1
                        let ni = split!!2
                        let inceput = split!!3
                        let end = split!!4

                        let row = "|"++ui ++ "|"++ profil ++ "|"++ni++ "|"++inceput++ "|"++end++ "|"
                        putStrLn row
                        putStrLn "|--------------------------------------------------------------------------------------------------------------------------|"
                        readDataFromStudii fileHandle
    
    readDataFromExperiente fileHandle = 
        do 
            isFileEnd <- hIsEOF fileHandle
            if isFileEnd 
                then
                    
                    return ("")
                else
                    do
                        info <- hGetLine  fileHandle
                        -- putStrLn info
                        let split = splitOn "," info
                        let tip = split!!0
                        let firma = split!!1
                        let perioada = split!!2
                        let row = "|"++tip ++ "|"++ firma ++ "|"++perioada++ "|"
                        putStrLn row
                        putStrLn "|------------------------------|"
                        readDataFromExperiente fileHandle
    
    readDataFromContact fileHandle = 
        do 
            isFileEnd <- hIsEOF fileHandle
            if isFileEnd 
                then
                    
                    return ("")
                else
                    do
                        info <- hGetLine  fileHandle
                        -- putStrLn info
                        let split = splitOn "," info
                        let tel = split!!0
                        let fax = split!!1
                        let email = split!!2
                        let row = "|"++tel ++ "|"++ fax ++ "|"++email++ "|"
                        putStrLn row
                        putStrLn "|------------------------------|"
                        readDataFromContact fileHandle
    
    createTabelAngajati::String->IO()
    createTabelAngajati fileName =do
            putStrLn "|---------------------------------------|"
            putStrLn "| Nume     |    Prenume     | UID       |"
            putStrLn "|---------------------------------------|"
            fileHandle <- openFile fileName ReadMode

            readDataFromA fileHandle
            putStrLn ""
    
    createTabelStudii::String->IO()
    createTabelStudii fileName =do
            putStrLn "|--------------------------------------------------------------------------------------------------------------------------|"
            putStrLn "| Unitate de invatamant   |  Profil   | Nume institutie   |     Data inceperi studiilor   |   Data finalizarii studiilor   |"
            putStrLn "|--------------------------------------------------------------------------------------------------------------------------|"
            fileHandle <- openFile fileName ReadMode

            -- readDataFromStudii fileHandle
            readDataFrom fileHandle
            putStrLn ""
    
    createTabelExperiente::String->IO()
    createTabelExperiente fileName =do
            putStrLn "|------------------------------|"
            putStrLn "| Tip   |  Firma   | Perioada  | "
            putStrLn "|------------------------------|"
            fileHandle <- openFile fileName ReadMode

            -- readDataFromExperiente fileHandle
            readDataFrom fileHandle
            putStrLn ""
    createTabelContact::String->IO()
    createTabelContact fileName =do
            putStrLn "|------------------------------|"
            putStrLn "| Telefon   |  Fax   | Email   | "
            putStrLn "|------------------------------|"
            fileHandle <- openFile fileName ReadMode

            readDataFromContact fileHandle
            -- readDataFrom fileHandle
            putStrLn ""
    createTabelVenit::String->IO()
    createTabelVenit fileName =do
            putStrLn "|--------|"
            putStrLn "| Suma   | "
            putStrLn "|--------|"
            fileHandle <- openFile fileName ReadMode

            readDataFrom fileHandle
            putStrLn ""
    
    createTabelInfo::String->IO()
    createTabelInfo fileName =do
            putStrLn "|-------------------------------------------|"
            putStrLn "| Cnp   |  Serie   | Data nastere  | iban   | "
            putStrLn "|-------------------------------------------|"
            fileHandle <- openFile fileName ReadMode

            readDataFromInfo fileHandle
            putStrLn ""
            