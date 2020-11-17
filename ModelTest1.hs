-- EX1 -----------------------------------

sfChr :: Char -> Bool
sfChr c = (c == '.' || c == '?' || c == '!' || c == ':')

nrProp :: [Char] -> Int
nrProp [] = 0
nrProp p = if sfChr (head p) then 
    (nrProp (tail p) + 1) else
        nrProp (tail p)

-- EX2  ---------------------------------

liniiN :: [[Int]] -> Int -> Bool
liniiN m n =  length (filter (\x -> length (filter (\y -> y <= 0) x) /= 0) (filter (\x -> length x == n) m)) == 0

-- EX3  ----------------------------------

data Punct = Pt [Int] deriving Show

data Arb = Vid | F Int | N Arb Arb deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a
    
instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (h:t)) = N (F h) (toArb (Pt t))  
    fromArb Vid = (Pt []) 
    fromArb (F x) = (Pt [x])
    fromArb (N (F x) a2) = 
        let (Pt y) = (fromArb a2) in
            Pt (x:y)


