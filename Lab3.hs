divizori n = [x | x<-[1..n], mod n x == 0]
prim n = (divizori n) == [1,n]
toatePrime n = [x | x<-[1..n], prim x]

myzip3 [] _ _ = []
myzip3 _ _ [] = []
myzip3 _ [] _ = []
myzip3 (ha:ta) (hb:tb) (hc:tc) = (ha,hb,hc):(myzip3 ta tb tc)

myZip3' l1 l2 l3 =
    let l = zip (zip l1 l2) l3
    in [(x,y,z) | ((x,y),z) <- l]

h x = (x,x)
i (x, y) = x * y

aplicaCu3 f = f 3