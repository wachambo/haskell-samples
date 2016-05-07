raices :: (Float,Float,Float) -> (Float,Float)
raices (a,b,c)
    | (d >= 0)  = ( (-b+rd) / da , (-b-rd) / da )
    | otherwise = error "discriminante negativo"
        where
        d  = b*b - 4*a*c
        rd = sqrt d
        da = 2*a

--ej3
tresDiferentes :: Int->Int->Int->Bool
tresDiferentes x y z = (x/=y) && (x/=z) && (y/=z)



--ej4
max3 :: Int->Int->Int->Int
max3 x y z = max x (max y z)



--ej5
cuantosIguales :: Int->Int->Int->Int
cuantosIguales x y z
    | (x==y) && (x==z) = 3
    | (x==y)           = 2
    | (x==z)           = 2
    | (y==z)           = 2
    | otherwise        = 1




--ej6
sumatorio :: Int->Int
sumatorio 1 = 1
sumatorio n = n + sumatorio(n-1)



sumatFinal :: Int->Int
sumatFinal n = gsumatorio n 0
    where
    gsumatorio :: Int->Int->Int
    gsumatorio 0 x = x
    gsumatorio n x = gsumatorio (n-1)(x+n)




--ej7
type Complejo = (Float,Float)
raices2 :: Float->Float->Float->[Complejo]
raices2 0 0 0 = error "no solucion"
raices2 0 b c = [( -c/b,0 )]
raices2 a b c
    | (d>0)  = [( (-b+rd/da),0 )  , ( (-b-rd/da),0 )]
    | (d==0) = [( (-b/da),0 )]
    | (d<0)  = [( (-b/da),rd/da ) , ( (-b/da),-rd/da )]
        where
        d = b*b - 4*a*c
        rd | (d>0) = sqrt(d)
           | (d<0) = sqrt(-d)
        da = 2*a




--ej10
prodEscalar :: [Int]->[Int]->Int
prodEscalar [] ys         = 0
prodEscalar xs []         = 0
prodEscalar (x:xs) (y:ys) = (x*y) + prodEscalar xs ys




--ej11
concatena :: [[Int]]->[Int]
concatena []       = []
concatena (xs:xss) = xs ++ concatena xss




concatena2 :: [[Int]]->[Int]
concatena2 []           = []
concatena2 ([]:yss)     = concatena2 yss
concatena2 ((x:xs):yss) = x:concatena2 (xs:yss)




--ej12
orden :: [Int]->Bool
orden []        = True
orden (x:[])    = True --tambien orden [x] = True
orden (x:y:xss) = (x<=y) && orden xss




--ej13
impares :: [Int]->[Int]
impares []      = []
impares (x:xs)
    | even x    = impares xs
    | otherwise = x:impares xs




--ej14
pp :: [Int]->[Int]
pp []           = []
pp (x:xs)
    | even x    = x:pp xs
    | otherwise = pp xs ++ [x]




pp2 :: [Int]->[Int]
pp2 xs = let (as,bs) = aux xs
         in as++bs
    where
    aux []     = ([],[])
    aux (x:xs) = let (p,i) = aux xs
                 in if even x then (x:p,i)
                    else           (p,x:i)



pp3 :: [Int]->[Int]
pp3 xs = aux2 (aux xs)
    where
    pon x (p,i)
        | even x    = (x:p,i)
        | otherwise = (p,x:i)

    aux []     = ([],[])
    aux (x:xs) = pon x (aux xs)

    aux2 :: ([Int],[Int])->[Int]
    aux2 (p,i) = p++i


pp4 :: [Int]->[Int]
pp4 xs = (\(p,i) -> p++i) (aux xs)
    where
    aux[]      = ([],[])
    aux (x:xs) = ( \(p,i) -> if even x then (x:p,i)
                             else           (p,x:i) ) (aux xs)
