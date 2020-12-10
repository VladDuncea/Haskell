import Data.Text

main :: IO ()
main = do
    linia1 <- getLine
    linia2 <- getLine
    let x = (read linia1) :: Int
    let y = (read linia2) :: Int
    print (x + y)
    putStrLn "Hello World!"

--Ex1

citeste 0 = return []
citeste n = do
    nume <- getLine
    varsta <- readLn :: (IO Int)
    restul <- citeste (n-1)
    return ((varsta,nume):restul)


ex1 = do
    n <- readLn :: (IO Int)
    date <- citeste n
    let (varsta,nume) = maximum date
    print (varsta, nume)

