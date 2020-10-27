import   Data.List

-- Ex din timpul laboratorului
h x = (x,x)
i (x, y) = x * y

aplicaCu3 f = f 3

-- Fisier profa

-- L3.1 

factori :: Int -> [Int]
factori x = [z | z<-[1..x], mod x z == 0]

prim :: Int -> Bool
prim x = (factori x) == [1,x]

numerePrime :: Int -> [Int]
numerePrime x = [z | z<-[2..x], prim z]

-- L3.2

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 [] _ _ = []
myzip3 _ _ [] = []
myzip3 _ [] _ = []
myzip3 (ha:ta) (hb:tb) (hc:tc) = (ha,hb,hc):(myzip3 ta tb tc)


myZip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3' l1 l2 l3 =
    let l = zip (zip l1 l2) l3
    in [(x,y,z) | ((x,y),z) <- l]

--------------------------------------------------------
----------FUNCTII DE NIVEL INALT -----------------------
--------------------------------------------------------
aplica2 :: (a -> a) -> a -> a
--aplica2 f x = f (f x)
--aplica2 f = f.f
--aplica2 f = \x -> f (f x)
aplica2  = \f x -> f (f x)

-- L3.3

--firstEl [ ('a', 3), ('b', 2), ('c', 1)]
firstEl :: [(b1, b2)] -> [b1]
firstEl l = map (\(x,_) -> x) l

-- sumList [[1, 3],[2, 4, 5], [], [1, 3, 5, 6]]
sumList :: [[Int]] -> [Int]
sumList l = map(\x -> sum x) l

-- prel2 [2,4,5,6]
prel2 ::  [Int] -> [Int]
prel2 l = map (\x -> if x `mod` 2 == 0 then x `div` 2 else x * 2 ) l


-- L3.4

--1
caracterLista :: (Foldable t, Eq a) => a -> [t a] -> [t a]
caracterLista c l = filter (elem c) l

--2
patrateImpare :: (Integral b) => [b] -> [b]
patrateImpare l = map (\x -> (x^2)) (filter odd l)

--3
patratePozImpare :: (Integral b) => [b] -> [b]
patratePozImpare l = map (\(x,_) -> (x^2)) (filter (\(_,y) -> (odd y)) (zip l [1..]))

--4
numaiVocale :: [[Char]] -> [[Char]]
numaiVocale l = map (\prop -> filter ( `elem` "aeiouAEIOU") prop) l

-- L3.5
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (h:t) = if (f h) then (h:(myfilter f t)) else (myfilter f t)

mymap :: (t -> a) -> [t] -> [a]
mymap _ [] = []
mymap f (h:t) = ((f h):(mymap f t))

--1
ordonataNat :: Ord a => [a] -> Bool
ordonataNat [ ] = True
ordonataNat [ _ ] = True
ordonataNat (x:xs) = and [ x < y | (x,y)<- zip (x:xs) (xs)]

--2
ordonataNat1 :: Ord a => [a] -> Bool
ordonataNat1 [ ] = True
ordonataNat1 [ _ ] = True
ordonataNat1 (x:y:xs) = x < y && ordonataNat xs 

--3
ordonata :: [ a ] -> ( a -> a -> Bool) -> Bool
ordonata (x:xs) f = and [ f x y | (x,y)<- zip (x:xs) (xs)]

(*<*) :: ( Integer , Integer ) -> ( Integer , Integer ) -> Bool
(*<*) (a,b) (c,d) = a < b && c < d

--4
compuneList :: (b -> c) -> [( a -> b )] -> [( a -> c )]
compuneList f lf = map (\ f' -> (f.f') ) lf

aplicaList :: a -> [(a->b)] -> [b]
aplicaList a lf = map (\ f -> f a) lf

myZip3 l1 l2 l3 = map (\ ((x,y),z) -> (x,y,z)) (zip (zip l1 l2) l3)