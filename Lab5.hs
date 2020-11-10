-- http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/
import Data.Char
import Data.List


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n l = 
    if n < 0  || n > (length l) 
        then error "Argument invalid"
        else (drop n l) ++ (take n l)

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3.
makeKey :: Int -> [(Char, Char)]
makeKey n = zip ['A'..'Z'] (rotate n ['A'..'Z']) 

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c l = 
    let nl = filter (\ (x,_) -> x == c) l
    in if (length nl) == 1
        then snd (head nl)
        else c

-- 5.
encipher :: Int -> Char -> Char
encipher n c = lookUp c (makeKey n)

-- 6.
normalize :: String -> String
normalize l = map (\x -> toUpper x) (filter (\x -> isLetter x || isDigit x) l)

-- 7.
encipherStr :: Int -> String -> String
encipherStr n l =  map (\x -> encipher n x) (normalize l) 

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey l = [(x,y) | (y,x) <- l]

-- 9.
decipher :: Int -> Char -> Char
decipher n c = lookUp c (reverseKey (makeKey n)) 

decipherStr :: Int -> String -> String
decipherStr n l = map (\x -> decipher n x) (filter (\x -> elem x ['A'..'Z'] || isDigit x || x == ' ') l)