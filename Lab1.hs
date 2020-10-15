import Data.List

-- EX2
myInt = 55555555555555555555555555555555555555555555555555555555555
double :: Integer -> Integer
double x = x+x

-- EX3
triple :: Integer -> Integer
triple x = 3*x

-- EX4
maxim :: Integer -> Integer -> Integer
maxim x y = 
    if ( x > y ) 
        then x 
        else y

maxim3 x y z = 
    if ( x > y && x > z)
        then x
        else if ( y > z)
            then y
            else z

maxim4 x y z k =
    let h = maxim3 x y z
    in maxim k h

--EX6
sumpatr x y = x*x + y*y

checkparity x = 
    if  mod x 2 == 0
        then "par"
        else "impar"
