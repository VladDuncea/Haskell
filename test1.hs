import Data.Char
inPasareascaChar :: Char -> [Char]
inPasareascaChar c = 
    let vocale = "aeiou" in --lista de vocale pentru a ne fi usor sa verificam
    if (elem c vocale) then
        [c]++"p"++[c]   --adaugam p si caracterul
    else
        [c] --intoarcem caracterul ca un string

inPasareasca :: [Char] -> [Char]
inPasareasca cuv = concat [inPasareascaChar x | x <- cuv ]



sumProd ::[Int] -> [Int] -> Int
sumProd x y = 
    if (length x) /= (length y) then    -- verificare lungimi diferite
        error "Listele au dimensiuni diferite!" --aruncam eroare
    else   
        foldr (+) 0 (map (\(x,y) -> (x*x) * (y*y) ) (zip x y) ) --lipim cu zip cele doua liste, apoi aplicam xi^2 * yi^2 si in final cu foldr facem suma produselor



data Enciclopedie = Intrare String String
     | Lista String [Enciclopedie]
   deriving Show

enc1 = Lista "animal"[Lista "mamifer"[Intrare "elefant" "acesta e un elefant", Intrare "caine" "acesta este un caine", Intrare "pisica" "aceasta este o pisica"], Intrare "animale domestice" "definitie"]

enc2 = Lista "Animal"[Lista "Mamifer"[Intrare "Elefant" "acesta e un elefant",Intrare "caIne" "acesta este un caine",Intrare "piSIca" "aceasta este o pisica"],Intrare "animale domestice" "definitie"]

enc3 = Lista "animal"[Lista "mamifer"[Intrare "elefant" "Acesta e un Elefant", Intrare "caine" "acesta este un caine", Intrare "pisica" "aceasta este o pisica"], Intrare "animale domestice" "definitie"]

enc4 = Lista "animal"[Lista "mamifer"[Intrare "pisica" "aceasta este o pisica",Intrare "elefant" "acesta e un elefant", Intrare "caine" "acesta este un caine"], Intrare "animale domestice" "definitie"]


lungime ::Enciclopedie -> Int
lungime (Intrare _ _) = 1 --Am ajuns la o intrare, deci o numaram ca 1
lungime (Lista _ l) = foldr (+) 0 ([lungime x | x<-l])  --sau cu sum

--auxiliar de toUpper pt un string
auxUpper :: [Char] -> [Char]
auxUpper s = [toUpper x | x<-s]

instance Eq Enciclopedie where
    (Intrare t1 d1) == (Intrare t2 d2) =  
        let verif1 = (auxUpper t1) == (auxUpper t2) in --verificam titlul intrarilor fara a tine cont de litere mari/mici
        let verif2 = d1 == d2 in --verificam definitia intrarii tinand cont de litere mari/mici
            verif1 && verif2
    (Lista t1 l1) == (Lista t2 l2) =
        let verif1 = (auxUpper t1) == (auxUpper t2) in --verificam titlul intrarilor fara a tine cont de litere mari/mici
        let verif2 = l1 == l2 in --comparam listele
            verif1 && verif2
