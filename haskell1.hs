-- Given three Integers, are all of them different?
allDiff :: Int->Int->Int->Bool
allDiff x y z = (x/=y) && (x/=z) && (y/=z)


-- Returns the maximun of three Integes.
max3 :: Int->Int->Int->Int
max3 x y z = max x (max y z)


--Given three Integers, how many are the same?
nequals :: Int->Int->Int->Int
nequals x y z
    | (x==y) && (x==z) = 3
    | (x==y)           = 2
    | (x==z)           = 2
    | (y==z)           = 2
    | otherwise        = 1


-- Given an Integer number, returns the summation from 1 to this number.
summation :: Int->Int
summation 1 = 1
summation n = n + summation(n-1)

summation2 :: Int->Int
-- tail recursive.
summation2 n = gsummation n 0
    where
    gsummation :: Int->Int->Int
    gsummation 0 x = x
    gsummation n x = gsummation (n-1)(x+n)


-- Returns the roots of a cuadratic equation.
roots :: (Float,Float,Float) -> (Float,Float)
-- Real numbers.
roots (a,b,c)
    | (d >= 0)  = ( (-b+rd) / da , (-b-rd) / da )
    | otherwise = error "COMPLEX ROOTS"
        where
        d  = b*b - 4*a*c
        rd = sqrt d
        da = 2*a

type Complex = (Float,Float)
roots2 :: Float->Float->Float->[Complex]
-- Real and Complex numbers.
roots2 0 0 0 = error "NO SOLUTION"
roots2 0 b c = [( -c/b,0 )]
roots2 a b c
    | (d>0)  = [( (-b+rd/da),0 )  , ( (-b-rd/da),0 )]
    | (d==0) = [( (-b/da),0 )]
    | (d<0)  = [( (-b/da),rd/da ) , ( (-b/da),-rd/da )]
        where
        d = b*b - 4*a*c
        rd | (d>0) = sqrt(d)
           | (d<0) = sqrt(-d)
        da = 2*a


-- Returns the scalar product of two Integer lists.
scalarProd :: [Int]->[Int]->Int
scalarProd [] ys         = 0
scalarProd xs []         = 0
scalarProd (x:xs) (y:ys) = (x*y) + scalarProd xs ys


-- Given a list of lists, returns a list which concatenate all the input lists.
join :: [[Int]]->[Int]
join []       = []
join (xs:xss) = xs ++ join xss

join2 :: [[Int]]->[Int]
join2 []           = []
join2 ([]:yss)     = join2 yss
join2 ((x:xs):yss) = x:join2 (xs:yss)


-- Is the list in ascending order?
asc :: [Int]->Bool
asc []        = True
asc (x:[])    = True --also asc [x] = True
asc (x:y:xss) = (x<=y) && asc xss


-- Given a list of Integers, returns only the odd.
odds :: [Int]->[Int]
odds []      = []
odds (x:xs)
    | even x    = odds xs
    | otherwise = x:odds xs


-- Sort the Integer list: first the even, then the odd
sort :: [Int]->[Int]
sort []           = []
sort (x:xs)
    | even x    = x:sort xs
    | otherwise = sort xs ++ [x]

sort2 :: [Int]->[Int]
sort2 xs = let (as,bs) = aux xs
         in as++bs
    where
    aux []     = ([],[])
    aux (x:xs) = let (p,i) = aux xs
                 in if even x then (x:p,i)
                    else           (p,x:i)

sort3 :: [Int]->[Int]
sort3 xs = aux2 (aux xs)
    where
    pon x (p,i)
        | even x    = (x:p,i)
        | otherwise = (p,x:i)

    aux []     = ([],[])
    aux (x:xs) = pon x (aux xs)

    aux2 :: ([Int],[Int])->[Int]
    aux2 (p,i) = p++i

sort4 :: [Int]->[Int]
sort4 xs = (\(p,i) -> p++i) (aux xs)
    where
    aux[]      = ([],[])
    aux (x:xs) = ( \(p,i) -> if even x then (x:p,i)
                             else           (p,x:i) ) (aux xs)
