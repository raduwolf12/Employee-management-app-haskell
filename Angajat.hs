module Angajat where
    import System.IO  
    import System.Directory
    import Data.List  
    import Data.List.Split


    data Angajat = Angajat {nume :: String  
                     , prenume :: String  
                     , uid :: String
                     , contact::Contact
                     , experienta::[Experienta]
                     ,infopersonala::InfoPersonala
                     ,studii::[Studii]
                     ,venit::Venituri
                     } deriving (Show)
    
    data Angajat1 = Angajat1 {nume1 :: String  
                     , prenume1 :: String  
                     , uid1 :: String
                     } deriving (Show)
    
    data Contact = Contact {telefon :: String  
                     , fax :: String  
                     , email :: String
                     } deriving (Show) 
    data Experienta = Experienta {tip :: String  
                     , firma :: String  
                     , perioada :: String
                     } deriving (Show)
    data InfoPersonala = InfoPersonala {cnp :: String  
                     , serie :: String  
                     , datanastere :: String
                     , iban :: String
                     } deriving (Show)
    data Studii = Studii {invatamant :: String  
                     , profil :: String  
                     , denumire :: String
                     , inceput :: String
                     , sfarsit :: String
                     } deriving (Show)
    data Venituri = Venituri {suma :: String
                     } deriving (Show) 

    getAngajatFromString :: [Char] -> Maybe Angajat1
    getAngajatFromString angajatStr = do
        let split = splitOn "," angajatStr
        let nume = split!!0
        let prenume = split!!1
        let cnp = split!!2
        let angajatNou = Angajat1{nume1 = nume, prenume1 = prenume, uid1 = cnp}
        return angajatNou

    formatAngajat :: Angajat1 -> IO()
    formatAngajat Angajat1 {nume1 = n
                        , prenume1 = p
                        , uid1 = a} = do
                            putStrLn a
                              
