-- Given two Integers, returns a Boolean.
f :: Int->Int->Bool
f x y = if (even(x+y)) then True else False

-- Given a function (Integer to Integer) returns a Boolean.
g :: (Int->Int)->Bool
g fun = if (fun(2) > fun(1)) then True else False


-- Get the remainder of a Integer division.
rest :: Int->Int->Int
rest x y
    | x < y  = x
    | x >= y = rest (x-y) y


-- GCD by Euclidean division using 'rest'.
mcd :: Int->Int->Int
mcd a b
    | b==0 = a
    | b/=0 = mcd b (rest a b)


-- GCD by Euclidean division using 'divisions'.
mcd2 :: Int->Int->Int
mcd2 a b
    | b==0 = a
    | b/=0 = mcd b (mod a b)


-- Primality test.
prime :: Int->Bool
prime x 
    | x==1   = False
    | x==2   = True
    | even x = False
    | x>2    = pri x 3 (isqrt x 1)
    where
        pri :: Int->Int->Int->Bool
        pri x i r
            | i>r            = True
            | (x`mod`i == 0) = False
            | otherwise      = pri x (i+2) r
        -- Integer square root
        isqrt :: Int->Int->Int
        isqrt n i
            | (i2 >= n) = i
            | otherwise = isqrt n (i+1)
            where i2 = i*i


-- Minimun prime bigger than a given Integer.
primeMin :: Int->Int
primeMin n
    | n==1      = 2
    | prime n   = n 
    | otherwise = if (even n) then primeMin (n+1) else primeMin (n+2)

    --NOTE: lambda-abstraction: (\n->primeMin n)


-- Fibonacci.
fib :: Int->Int -- O(2^n)
fib 0 = 1
fib 1 = 1 
fib n = fib(n-1) + fib(n-2)


fib2 :: Int->Int -- O(n)
fib2 n = gfib 0 1 n
    where
    gfib :: Int->Int->Int->Int
    gfib a b n
        | n==0 = b
        | n>0  = gfib b (a+b) (n-1)

--fib: 1 1 2 3 5 8 13 ...

--(1,1) --> (1,2)
--(1,2) --> (2,3)
--(2,3) --> (3,5)

--(a,b) --> (b,a+b)


-- Remove repeated elements of a list.
rmRep :: Eq a=>[a]->[a]
rmRep []     = []
rmRep (x:xs)
    | (elem x xs) = rmRep xs
    | otherwise   = x:rmRep xs


-- Remove repeated neighbouring elements of a list.
rmRepN :: Eq a=>[a]->[a]
rmRepN []       = []
rmRepN [x]      = [x]
rmRepN (x:y:ys) = if (x==y) then rmRepN (y:ys)
                  else           x:rmRepN (y:ys)


-- Given two lists, return a list with the elements from the first one but not from the second one.
diff :: [Int]->[Int]->[Int]
diff [] ys     = []
diff (x:xs) ys
    | (elem x ys)    = diff xs ys
    | otherwise      = x:diff xs ys

-- Improved for sorted lists:
diff2 :: [Int]->[Int]->[Int]
diff2 [] ys         = []
diff2 (x:xs) (y:ys)
    | x<y            = x:diff2 xs (y:ys)
    | x==y           = diff2 xs ys
    | x>y            = diff2 (x:xs) ys 


-- Returns the positions of a element in a list.
positions :: Eq a=>a->[a]->[Int]
positions x [] = []
positions x xs = pos x xs 1
    where
    pos :: Eq a=>a->[a]->Int->[Int]
    pos x [] c      = []
    pos x (y:ys) c
        | x==y      = c:pos x ys (c+1)
        | otherwise = pos x ys (c+1)


-- Infix operator that given a list of functions and a value, returns a list resulting from applying each function with the value.
infixl 6 |>
(|>) :: [a->b]->a->[b]
[] |> x     = []
(f:fs) |> x = (f x):(fs |> x)


-- Is the first list a Sublist of the second list?
sublist :: Eq a=>[a]->[a]->Bool
sublist [] _          = True
sublist _ []          = False
sublist (x:xs) (y:ys)
    | x==y             = sublist xs ys
    | otherwise        = sublist (x:xs) ys


-- Is the first list Subsequence of the second list?
-- Subsequence is sublist and also the elements are contiguous.
subseq :: Eq a=>[a]->[a]->Bool
subseq [] _          = True
subseq _ []          = False
subseq (x:xs) (y:ys)
    | x==y           = subseqAux xs ys
    | otherwise      = subseq (x:xs) ys
    where
    subseqAux :: Eq a=>[a]->[a]->Bool
    subseqAux [] _          = True
    subseqAux _ []          = False
    subseqAux (x:xs) (y:ys)
        | x==y              = subseqAux xs ys
        | x/=y              = False


-- Returns a list of all possible permutations given a list.
permute :: [a]->[[a]]
permute []     = [[]]
permute (x:xs) = concat( map (x-->) (permute xs) )
    where
    -- Insert an element in every possible position of a list.
    infixl 6 -->
    (-->) :: a->[a]->[[a]]
    e --> []     = [[e]]
    e --> (y:ys) = (e:y:ys):map (y:) (e-->ys)
