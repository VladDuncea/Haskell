data Stiva a = Goala | Ocupat a (Stiva a)

push :: Stiva a -> a -> Stiva a
push stiva elem = Ocupat elem stiva

pop :: Stiva a -> (a, Stiva a)
pop Goala = error "Stiva e goala"
pop (Ocupat elem tail) = (elem, tail)

instance Show a => Show (Stiva a) where
    show Goala = "[]"
    show (Ocupat elem tail) = show elem ++ ":" ++show tail

plus :: Num a => Stiva a -> Stiva a
plus stiva = 
    let (x, stiva2) = pop stiva in
    let (y, stiva3) = pop stiva2 in
    push stiva3 (x+y)

first :: (a -> Bool) -> Stiva a -> (a, Stiva a)
first conditie stiva = 
    let (x, stiva2) = pop stiva in
    if conditie x   then (x, stiva2)
                    else first conditie stiva2

convert [] = Goala
convert (h:t) = Ocupat h (convert t)


test stiva =
    let (x, stiva2) = pop stiva in
    let (y, stiva3) = pop stiva2 in
    let (z, stiva4) = pop stiva3 in
    if(x+y+z) > 10 then
        pop stiva4
    else
        let(_, stiva5) = pop stiva4 in
            pop stiva5



newtype Plan a = P { runPlan :: Stiva String -> (a,Stiva String) }

pop' :: Plan String
pop' = P pop

push' :: String -> Plan ()
push' val = P (\stiva -> ((), push stiva val))

instance Functor Plan where
    fmap functie altPlan = P
        (\stiva -> let (ceva, stiva_noua) = runPlan altPlan stiva in
            (functie ceva, stiva_noua))

-- readint :: Plan Int
-- readint = P (\stiva -> let (elem, stiva2) = runPlan pop' stiva in
--                         (read elem, stiva2))

readint :: Plan Int
readint = fmap read pop'

-- patrat_int :: Plan Int 
-- patrat_int = P
--     (\stiva -> let (elem, stiva2) = runPlan readint stiva in
--         (elem^2, stiva2))

patratul_int = fmap (^2) readint

a = convert["2","4","6"]

instance Applicative Plan where
    pure x = P (\stiva -> (x, stiva))
    plan_de_functie <*> plan_de_elem = P
        (\stiva -> let (functia,stiva2) = runPlan plan_de_functie stiva in
                    let (elem, stiva3) = runPlan plan_de_elem stiva2 in
                        (functia elem, stiva3))

instance Monad Plan where
    (plan_de_elem) >>= functie_care_intoare_plan = P
        (\stiva -> let (elem, stiva2) = runPlan plan_de_elem stiva in
                let plan_nou_nout = functie_care_intoare_plan elem in
                    runPlan plan_nou_nout stiva2)

plus' :: Plan ()
plus' = do
    x <- pop'
    y <- pop'
    push' (x++y)

test' = do
    x <- readint
    y <- readint
    z <- readint
    if (x + y + z) > 10 then
        pop'
    else do
        pop' 
        pop'


aux_citit 0 = do
    return []
aux_citit n = do
    x <- readint
    restul <- aux_citit (n-1)
    return (x:restul)

suma_a_n = do
    n <- readint
    lista <- aux_citit n
    return (sum lista)

read' ::Read a => Plan a
read' = do
    x <- pop'
    return (read x)
    -- read' = fmap read pop'