data MyList a = Nil | a ::: (MyList a)

instance Show a => Show (MyList a) where
    show Nil = "[]"
    show (head ::: tail) = (show head) ++ ":" ++ (show tail)


instance Functor MyList where
    fmap _ Nil = Nil
    fmap f (head ::: tail) = (f head) ::: (fmap f tail)