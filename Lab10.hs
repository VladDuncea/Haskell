data Lista a = Gol | a ::: (Lista a)

infixr :::

instance Show a => Show (Lista a) where
    show Gol = "[]"
    show (head:::tail) = (show head) ++ " " ++ (show tail)

instance Functor Lista where
    fmap _ Gol = Gol
    fmap f (head:::tail) = (f head) ::: (fmap f tail)

instance Applicative Lista where
    pure x = x ::: (pure x)

    Gol <*> _ = Gol
    _ <*> Gol = Gol
    (headf ::: tailf) <*> (heade ::: taile) = (headf heade):::(tailf <*> taile)

instance Monad Lista where
    Gol >>= _ = Gol
    (head ::: tail) >>= op =
        let alta_cutie = op head in
            case alta_cutie of  Gol         -> tail >>= op
                                (h::: _)    -> h ::: (tail >>= op)




ma >>>> mb = do
    _ <- ma
    mb

(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> (c -> Maybe b)

(f <=< g) x = g x >>= (\a' -> f a')

divisori 0 = Nothing
divisori x = Just [y | y<- [2..(x-1)], x `mod` y ==0]



citeste_numere :: (Num a, Read a) => Int -> IO [a]
citeste_numere 0 = return []
citeste_numere n = do
    x <- readLn
    tail <- citeste_numere (n-1)
    return (x:tail)

plan_suma :: (Num a, Read a) => IO a
plan_suma = do
    n <- readLn :: IO Int
    lista_numere <- citeste_numere n
    return (sum lista_numere)


citeste_numere' 0 = return []
citeste_numere' n =
    readLn >>= (\x -> citeste_numere (n-1) >>= (\tail -> return (x:tail)))

plan_suma' =
    readLn >>= (\n -> citeste_numere n >>= (\lista_numere -> return (sum lista_numere)))