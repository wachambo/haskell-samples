-- Get k rows of Pascal's triangle.
-- >take k pascal
pascal :: [[Int]]
pascal = genRows [1]
    where
    genRows :: [Int]->[[Int]]
    genRows f = f:genRows(1:next f)

    next :: [Int]->[Int]
    next [x]      = [x]
    next (x:y:ys) = (x+y):next(y:ys)

-- Get the combinations of n choose k using the Pascal's triangle.
-- https://en.wikipedia.org/wiki/Pascal's_triangle
comb :: Int->Int->Int
comb n m = (pascal !! n) !! m


-- Compress a list returning a new list with tuples (e,n) where
-- 'e' represents the element and 'n' the number of consecutive apparences of 'e'.
compr :: Eq a=>[a]->[(a,Int)]
compr [] = []
compr (x:y:ys) = aux (x,1) (y:ys)
    where
    aux :: Eq a=>(a,Int)->[a]->[(a,Int)]
    aux (e,n) [] = [(e,n)]
    aux (e,n) (c:cs) = if(e==c) then aux (e,n+1) cs
                       else          (e,n):aux (c,1) cs


-- Given a 'n' Integer, returns all the Pytagorean tripes within the range [1, n].
-- A Pythagorean triples (a,b,c) are solutions to the Pythagorrean Theorem a^2 + b^2 = c^2
pyth :: Int->[(Int,Int,Int)]
-- To avoid symetrical solutions ([3,4,5] == [4,3,5] ...)
-- force a<b ('a' strictly less than 'b' because 2a^2 = c^2 is impossible!
pyth n = [(a,b,c) | a<-[1..n], b<-[1..n], c<-[1..n], a^2+b^2==c^2, a<b]


-- Polynomial data type may be represented as a list of coefficients:
-- 2 -5x + x^3 -> P[2,-5,0,1]
data Pol = P [Float]
    deriving Show

instance Eq Pol 
    where
    (P [])     == (P [])     = True
    (P [])     == (P (x:xs)) = (x==0) && (P []) == (P xs)
    (P (x:xs)) == (P [])     = (P []) == (P (x:xs))
    (P (x:xs)) == (P (y:ys)) = (x==y) && (P xs) == (P ys)
-- test: >P[1,2,4]==P[1,2,3]
--       >False

-- Evaluate a polynomial in a given value
eval ::Pol->Float->Float
eval (P p) x = foldr (\a ac -> a+x*ac) 0 p
-- test >evalua (P[1,2,3]) 3
--      >34.0
-- Dont forgive paretheses!
