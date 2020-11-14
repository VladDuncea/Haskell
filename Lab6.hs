import Data.List (nub)
import Data.Maybe (fromJust)

-- EX 1 -----------------------------------------------------------
data Fruct
    = Mar String Bool
    | Portocala String Int
      deriving(Show)


ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False, Portocala "Sanguinello" 10, Portocala "Valencia" 22, Mar "Golden Delicious" True, Portocala "Sanguinello" 15, Portocala "Moro" 12, Portocala "Tarocco" 3, Portocala "Moro" 12, Portocala "Valencia" 2, Mar "Golden Delicious" False, Mar "Golden" False, Mar "Golden" True]


ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala s _) = (s == "Tarocco" || s == "Moro" || s == "Sanguinello")
ePortocalaDeSicilia _ = False


test_ePortocalaDeSicilia1 = ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 = ePortocalaDeSicilia (Mar "Ionatan" True) == False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum (map (\(Portocala _ f) -> f) (filter (\x -> ePortocalaDeSicilia x) l))

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

eMarCuVierme :: Fruct -> Bool
eMarCuVierme (Mar _ True) = True
eMarCuVierme _ = False

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = sum (map (\x -> if eMarCuVierme x then 1 else 0) l )

test_nrMereViermi = nrMereViermi listaFructe == 2

-- EX 2 -----------------------------------------------------------

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _ ) = "Woof!"

rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ r) = Just r

-- II --------------------------------------------------

type Nume = String
data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: ((Not (Var "P")) :&: (Not (Var "Q")))

p3 :: Prop
p3 = ( Var "P" :&: (Var "Q" :|: Var "R")) :&: ( ((Not (Var "P")) :|: (Not (Var "Q"))) :&: ((Not (Var "P")) :|: (Not (Var "R"))))

-- EX2 -------------------------------------------------------

instance Show Prop where
    show (Var s) = s
    show (Not p) = "(~" ++ show p ++ ")"
    show (p1 :|: p2) = "(" ++ show p1 ++ "|" ++ show p2 ++ ")"
    show (p1 :&: p2) = "(" ++ show p1 ++ "&" ++ show p2 ++ ")"

test_ShowProp :: Bool
test_ShowProp = show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

-- EX3 ----------------------------------------------------------

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var x) env = impureLookup x env 
eval (Not x) env = not (eval x env)
eval (p1 :|: p2) env = (eval p1 env) || (eval p2 env) 
eval (p1 :&: p2) env = (eval p1 env) && (eval p2 env) 

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

-- EX4 -------------------------------------------------

variabile :: Prop -> [Nume]
variabile (Var x) = [x]  
variabile (Not x) = variabile x
variabile (p1 :|: p2) = nub (variabile p1 ++ variabile p2)
variabile (p1 :&: p2) = nub (variabile p1 ++ variabile p2)

test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

-- EX5 ----------------------------------------------------

envs :: [Nume] -> [Env]
envs [] = [[]]
envs (x:xs) = [(x,False ):e | e <- envs xs ] ++ [( x , True ):e | e <- envs xs ]

test_envs =
      envs ["P", "Q"]
      ==
      [ [ ("P",False)
        , ("Q",False)
        ]
      , [ ("P",False)
        , ("Q",True)
        ]
      , [ ("P",True)
        , ("Q",False)
        ]
      , [ ("P",True)
        , ("Q",True)
        ]
      ]

-- EX6 ----------------------------------------------------

satisfiabila :: Prop -> Bool
satisfiabila  p = 
  let vars = variabile p in
  let l = envs vars in
    elem True [eval p x | x <- l]


test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

valida :: Prop -> Bool
valida p = not (satisfiabila (Not p))

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True