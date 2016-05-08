-- Insert an element in a sorted list.
insert :: Ord a=>a->[a]->[a]
insert e = foldr f [e]
    where
    f :: Ord a=>a->[a]->[a]
    f x (y:ys) = if (x>y) then y:x:ys
                 else          x:y:ys


-- Given a list of lists, returns the minumun of the max value of each list.
minMax :: Ord a=>[[a]]->a
minMax (xs:xss) = foldl f (may xs) xss
    where
    may :: Ord a=>[a]->a 
    may (x:xs) = foldl max x xs
    --may [x] = x
    --may (x:xs) = if (x>may xs) then x else may xs
    f :: Ord a=>a->[a]->a 
    f ac xs = min ac (may xs)  


-- Returns an infinite list of lists [(n,0), (n-1,1), ..., (0,n)]
comb :: [[(Int,Int)]]
comb = map list [0..]
    where
    list :: Int->[(Int,Int)]
    list n = [(n-a,a) | a<-[0..n]]

-- Returs a list of tuples (a,b) such as a+b=v given 'v'
sums :: Int->[(Int,Int)]
sums v = comb !! v


-- Hamming numbers are multiples of 2,3 or 5
-- https://en.wikipedia.org/wiki/Regular_number
hamming :: [Int]
hamming = 1:[n | n<-[2..], n`mod`2==0 || n`mod`3==0 || n`mod`5==0]


-- Remove repeated neighbouring elements of a list.
rmRepN :: Eq a=>[a]->[a]
rmRepN = foldr f []
    where
    f :: Eq a=>a->[a]->[a]
    f x []                    = [x]
    f x (y:ys) = if (x==y) then y:ys
                 else           x:y:ys


-- Set data type
-- {1,2,3} -> C[1,2,3] or C[3,2,1,3,2] ...
data Set a = C [a]
    deriving Show

instance Eq a=>Eq (Set a)
    where
    (C [])     == (C []) = True
    (C (x:xs)) == (C ys) = (elem x ys) && (C (filter (/=x) xs)) == (C (filter (/=x) ys))
    (C _)      == (C _)  = False

-- Set intersection
-- test: >inter (C [1,2,3,4,5]) (C [0,2,8,9,4,10])
inter :: Eq a=>Set a->Set a->Set a
inter (C xs) (C ys) = C [x | x<-xs, elem x ys]

-- Remove repeated elements in the Set.
-- test: >compact (C [1,2,1,3,1,1,2,4,5,4])
compact :: Eq a=>Set a->Set a
--compact (C []) = C []
--compact (C (x:xs)) = C (x:compact (C (filter (/=x) xs)))   -- Cannot stick 'x' to Set!!
compact (C c) = C (aux c)
    where
    aux :: Eq a=>[a]->[a]
    aux []     = []
    aux (x:xs) = x:aux (filter (/=x) xs)
