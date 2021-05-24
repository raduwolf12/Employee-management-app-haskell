module HSFSDF where
  import System.Console.ANSI

  import System.IO

  -- Valori inregistrare
  type TInrVC=[String]

  --Date formatare 
  type TInrFC=[(Int,Int)]

  -- Determinare lungime inregistrare
  -- Suma lungimii zonelor de afisare
  -- Fara barele verticale
  lunRec::TInrFC->Int
  lunRec []=0
  lunRec ((lza,_):rldf)=lza+(lunRec rldf)

  -- Replicare caracter specificat de un numar de ori
  repCar::Char->Int->String
  repCar _ 0=[]
  repCar car ncr=[car]++(repCar car (ncr-1))

  -- Determinare numar de spatii de completare
  -- Daca lza-(length dcol)<=0 numarul spatiilor decompletare
  -- este setat la 0
  detNSS::String->Int->Int
  detNSS dcol lza |(lza-(length dcol)>0)=lza-(length dcol)
                  |otherwise=0

  -- Generare subliniere cu un caracter specificat
  -- compatibila cu datele unui raport
  sublRap::Char->TInrFC->String
  sublRap _ []=[]
  sublRap car ldf=(repCar car ((length ldf)+1))++(repCar car (lunRec ldf)) 

  -- Afisare coloane inregistrare sau antet
  rType::Int->Int->TInrVC->TInrFC->Int->IO()
  rType _ _ [] [] _=return ()
  rType lc cs (dcol:rlvc) ((lza,ca):rldf) csnm=do
  {
    setCursorPosition lc cs;
    putStr "|";
    hFlush stdout;
    cType lc (cs+1) dcol lza ca;
    rType lc (cs+lza+1) rlvc rldf csnm
  }

  -- Generare raport
  genSRap::Int->Int->TInrVC->[TInrVC]->TInrFC->IO()

  -- Oprire generare raport
  genSRap _ cs _ [] lDF=return ()

  -- Generare raport efectiv
  genSRap ls cs ldan ldi ldf=do{
    clearScreen;
    setCursorPosition ls cs;
    -- Afisare antet
    putStr (sublRap '-' ldf)>>hFlush stdout;
    rType (ls+1) cs ldan ldf cs;
    setCursorPosition (ls+2) cs;
    putStr (sublRap '-' ldf)>>hFlush stdout;  

    -- Afisare inregistrari
    lrType (ls+3) cs ldi ldf;

    Just (lc,_)<-getCursorPosition;
    setCursorPosition (lc+1) cs;
    putStr (sublRap '-' ldf)>>hFlush stdout;  
  }  
  
      
  -- Afisare coloana
  cType::Int->Int->String->Int->Int->IO()

  -- Aliniere stanga
  cType lc cs dcol lza 0=do{
    setCursorPosition lc cs;
    putStr dcol;
    hFlush stdout;
    nss<-return (detNSS dcol lza);
    ss<-return (repCar ' ' nss);
    putStr $ ss++"|";
    hFlush stdout}

  -- Afisare lista de inregistrari
  lrType::Int->Int->[TInrVC]->TInrFC->IO()
  lrType _ _ [] _=return ()
  lrType lc cs (inr:linr) ldf=do
  {
    rType lc cs inr ldf cs;
    lrType (lc+1) cs linr ldf
  }  


  -- Testare
  afisareAngajat::[String]->[[String]]->IO()
  afisareAngajat lDA lInr= do{
    -- lDA<-return ["Nume","Prenumse","UID"]
    -- Lista date formatare
    lDF<-return [(15,0),(20,0),(20,0)];

    -- Pregatire ecran pentru raport nou
    clearScreen;

    -- Lansare generare raport
    genSRap 4 20 lDA lInr lDF;

    t<-getLine;
    putStrLn t
  }
  -- main::IO()
  -- main=do
  --   forta ["Nume","Prenumse","Gemn","muie"]  [["Arsene","Lupin","f","f"],["Vlad","Georgescu","f","f"],["Buzea","Pompiliu","f","f"]]
    