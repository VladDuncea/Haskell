import Data.Char
--EX1
fibonacciCazuri n
    | n < 2 = n
    | otherwise = fibonacciCazuri(n - 1) + fibonacciCazuri(n - 2)

fibaux 0 = (0,1)
fibaux n =
    let (x,y) = fibaux (n-1) in (y, x + y)

fib n = fst(fibaux(n))

fibaux' a b 0 = a
fibaux' a b n = fibaux' b (a + b) (n-1)

fib' n = fibaux' 0 1 n
-- final EX1


f [] = 0
f lista =
    let h = head lista 
        t = tail lista in
        if mod h 2 == 0 then
            h + f t
        else
            f t

dublu [] = []
dublu lista =
    let h = head lista 
        t = tail lista in
    (2*h):(dublu t)

dublu' [] = []
dublu' (h:t) = (2 * h):(dublu' t)

suma2 [] = 0
suma2 [_] = 0
suma2 (x:y:_) = x + y

dublu2 l = [2*x | x <- l]

pare l = [x | x<-l, x `mod` 2 == 0]
sumapare :: Integral a => [a] -> a
sumapare l = sum(pare l)

prodcart a b = [(x,y) | x<-a, y<-b]

prim 0 = False
prim 1 = False
prim 2 = True
prim n = 
    let divizori = [d | d<-[2..(n-1)] , mod n d == 0] in
        divizori == []

suma_alat [] = []
suma_alat [x] = [x]
suma_alat (a:b:t) = (a+b):(suma_alat t)

--EX2
inIntervalRec ::Integral a => a -> a -> [a] -> [a]
inIntervalRec _ _ [] = []
inIntervalRec a b l 
    |   (h>=a) && (h<=b) = [h] ++ (inIntervalRec a b t)
    |   otherwise = inIntervalRec a b t
    where
        h = head l
        t = tail l 

inIntervalComp :: Ord a => a -> a -> [a] -> [a]
inIntervalComp a b l = [ x | x<-l, x<=b, x>=a]

--EX3
pozitiveRec  l 
    | null l = 0
    |   h > 0      = 1 + f
    |   otherwise = f
    where
        h = head l
        t = tail l 
        f = pozitiveRec t

pozitiveComp l = length [x | x<-l, x>0]

--EX4
pozitiiImpareRecAux poz l 
    |   null l      = l
    |   odd h       = [poz] ++ f
    |   otherwise = f
    where
        h = head l
        t = tail l 
        f = pozitiiImpareRecAux (poz+1) t

pozitiiImpareRec l = pozitiiImpareRecAux 0 l

pozitiiImpareComp l = [a | (a,b)<-zip [0..] l ,odd b]

--EX5
multDigitsRec l
    |   null l = 1
    |   isDigit h      =  (digitToInt h) * f
    |   otherwise = f
    where
        h = head l
        t = tail l 
        f = multDigitsRec t

multDigitsComp l = foldl (*) 1 [ digitToInt x | x<-l, isDigit(x)]

--EX6
discountRec l
    |   null l      = []
    |   h < 200     = [h] ++ f
    |   otherwise = f
    where
        h = 0.75 * (head l)
        t = tail l 
        f = discountRec t

discountComp l = [y |   x<-l,
                        let y = 0.75*x,
                        y<200 ]