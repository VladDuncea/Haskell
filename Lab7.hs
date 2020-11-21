import Data.List
import Data.Maybe

type Name = String

data  Value  =  VBool Bool
        |VInt Int
        |VFun (Value -> Value)
        |VError
        
data  Hask  = HTrue | HFalse 
        |HIf Hask Hask Hask
        |HLit Int
        |HLet Name Hask Hask
        |Hask :==: Hask
        |Hask :+:  Hask
        |Hask :*:  Hask
        |HVar Name
        |HLam Name Hask
        |Hask :$: Hask
        deriving (Read, Show)
        
infix 4 :==:
infixl 6 :+:
infixl 7 :*:
infixl 9 :$:

type  HEnv  =  [(Name, Value)]

showV :: Value -> String
showV (VBool b)   =  show b
showV (VInt i)    =  show i
showV (VFun _)    =  "<function>"
showV (VError)    =  "<error>"

eqV :: Value -> Value -> Bool
eqV (VBool b) (VBool c)    =  b == c
eqV (VInt i) (VInt j)      =  i == j
eqV (VFun _) (VFun _)      =  error "Unknown"
eqV (VError ) (VError)      =  error "Unknown"
eqV _ _               = False

hEval :: Hask -> HEnv -> Value
hEval HTrue r         =  VBool True
hEval HFalse r        =  VBool False

hEval (HIf c d e) r   =
  hif (hEval c r) (hEval d r) (hEval e r)
  where  hif (VBool b) v w  =  if b then v else w
         hif _ _ _ = VError   
  
hEval (HLit i) _      =  VInt i

hEval (HLet n h1 h2) r = 
  let val = hEval h1 r in
  let r2 = ((n,val):r) in
    hEval h2 r2

hEval (d :==: e) r     =  heq (hEval d r) (hEval e r)
  where  heq (VInt i) (VInt j) = VBool (i == j)
         heq  _ _ = error "Tipul de date nu poate fi comparat!"     
  
hEval (d :+: e) r    =  hadd (hEval d r) (hEval e r)
  where  hadd (VInt i) (VInt j) = VInt (i + j)
         hadd _ _  = error "Tipul de date nu poate fi adunat!"   

hEval (d :*: e) r    =  hmul (hEval d r) (hEval e r)
  where  hmul (VInt i) (VInt j) = VInt (i * j)
         hmul _ _  = error "Tipul de date nu poate fi inmultit!" 
  
hEval (HVar x) r      =  fromMaybe VError (lookup  x r)

hEval (HLam x e) r    =  VFun (\v -> hEval e ((x,v):r))


hEval (d :$: e) r    =  happ (hEval d r) (hEval e r)
  where  happ (VFun f) v  =  f v
         happ _ _ = VError

run :: Hask -> String
run pg = showV (hEval pg [])

h0 =  (HLam "x" (HLam "y" ((HVar "x") :+: (HVar "y") :*: (HVar "y") ))) 
      :$: (HLit 3)
      :$: (HLit 5)

h1 =  (HLam "x" (HLam "y" ((HVar "x") :*: HTrue))) 
      :$: (HLit 3)
      :$: (HLit 5)
      
h2 = HLet "x" (HLit 3) ((HLit 4) :+: (HVar "x"))

test_h0 = eqV (hEval h0 []) (VInt 7)