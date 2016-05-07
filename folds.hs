-- A bunch of functions expressed with the help of 'fold' function.

-- Keep the elemets that meet certain criteria.
filtr :: (a->Bool)->[a]->[a]
filtr p = foldl g []
    where g ac x = if (p x) then ac++[x] else ac


-- Reverse a list.
rev :: [a]->[a]
rev = foldl (\ac x -> x:ac) []


-- Calculates the length of a list.
len :: [a]->Int
len = foldl (\ac x -> ac+1) 0


-- Deletes all the occurrences of an element in a list.
rm :: Eq a=>a->[a]->[a]
rm m = foldr g []
    where g x ac = if (x==m) then ac else x:ac

-- Substract the second list from the first one.
resta :: Eq a=>[a]->[a]->[a]
resta xs ys         = foldl g xs ys
    where
    g [] _          = []
    g (x:xs) y
        | (x==y)    = xs
        | otherwise = x:(g xs y)

-- Clasify a list according to a predicate. If an element comply with it, goes to the first return list, else to the second return list.
partition :: (a->Bool)->[a]->([a],[a])
partition  p = foldl g ([],[])
    where g (i,d) x = if (p x) then (x:i,d) else (i,x:d)


-- Union and Intersection of lists.
union, inter :: Eq a=>[a]->[a]->[a]
union xs ys = foldl g xs ys
    where g ac y = if (elem y ac) then ac else y:ac

inter xs ys         = foldr h [] ys
    where
    h y ac
        | elem y xs = y:ac
        | otherwise = ac

-- Interperse a value between the elements of a list.
intersperse :: a->[a]->[a]
intersperse c = foldr (\x ac -> x:c:ac) []


-- Transpose a matrix.
-- Think a matrix as a list of rows(lists)
transpose :: [[a]]->[[a]]
transpose xss       = foldr g (take (length xss) (repeat [])) xss
    where
    g [] _          = []
    g (x:xs) (a:ac) = (x:a):g xs ac
