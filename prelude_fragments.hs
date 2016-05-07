infixr 9  .
infixl 9  !!
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`, :%, %
infixl 6  +, -
--infixr 5  :    -- this fixity declaration is hard-wired into Hugs
infixr 5  ++
infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`






(==), (/=) :: a -> a -> Bool
x == y      = not (x/=y)
x /= y      = not (x==y)

(<), (<=), (>=), (>)   :: a -> a -> Bool
x <= y                  = compare x y /= GT
x <  y                  = compare x y == LT
x >= y                  = compare x y /= LT
x >  y                  = compare x y == GT

max, min               :: a -> a -> a
max x y   | x <= y      = y
          | otherwise   = x
min x y   | x <= y      = x
          | otherwise   = y


pi                  :: a
exp, log, sqrt      :: a -> a
(**), logBase       :: a -> a -> a
sin, cos, tan       :: a -> a
asin, acos, atan    :: a -> a



even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even

-- Boolean type -------------------------------------------------------------

data Bool    = False | True
           deriving (Eq, Ord, Ix, Enum, Read, Show, Bounded)

(&&), (||)  :: Bool -> Bool -> Bool
False && x   = False
True  && x   = x
False || x   = x
True  || x   = True

not         :: Bool -> Bool
not True     = False
not False    = True

otherwise   :: Bool
otherwise    = True

-- Character type -----------------------------------------------------------

data Char               -- builtin datatype of ISO Latin characters
type String = [Char]    -- strings are lists of characters

isSpace, isDigit :: Char -> Bool

isSpace c  =  c == ' '  ||
              c == '\t' ||
              c == '\n' ||
              c == '\r' ||
              c == '\f' ||
              c == '\v' ||
              c == '\xa0'

isDigit c              =  c >= '0'   &&  c <= '9'

primitive isUpper      :: Char -> Bool
primitive isLower      :: Char -> Bool
primitive isAlpha      :: Char -> Bool
primitive isAlphaNum   :: Char -> Bool

-- Lists --------------------------------------------------------------------

-- data [a] = [] | a : [a] 

-- Tuples -------------------------------------------------------------------

-- data (a,b) = (a,b) 

-- Standard Integral types --------------------------------------------------

data Int      -- builtin datatype of fixed size integers
data Integer  -- builtin datatype of arbitrary size integers

absReal x
         | x >= 0    = x
         | otherwise = -x

signumReal x 
         | x == 0    =  0
         | x > 0     =  1
         | otherwise = -1

-- Standard Floating types --------------------------------------------------

data Float     -- builtin datatype of single precision floating point numbers
data Double    -- builtin datatype of double precision floating point numbers

-- Some standard functions --------------------------------------------------

fst            :: (a,b) -> a
fst (x,_)       = x

snd            :: (a,b) -> b
snd (_,y)       = y

id             :: a -> a
id    x         = x

(.)            :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x       = f (g x)

flip           :: (a -> b -> c) -> b -> a -> c
flip f x y      = f y x

until          :: (a -> Bool) -> (a -> a) -> a -> a
until p f x     = if p x then x else until p f (f x)

error          :: String -> a
error s         = throw (ErrorCall s)

head             :: [a] -> a
head (x:_)        = x

last             :: [a] -> a
last [x]          = x
last (_:xs)       = last xs

tail             :: [a] -> [a]
tail (_:xs)       = xs

init             :: [a] -> [a]
init [x]          = []
init (x:xs)       = x : init xs

null             :: [a] -> Bool
null []           = True
null (_:_)        = False

(++)             :: [a] -> [a] -> [a]
[]     ++ ys      = ys
(x:xs) ++ ys      = x : (xs ++ ys)

map              :: (a -> b) -> [a] -> [b]
map f xs          = [ f x | x <- xs ]

filter           :: (a -> Bool) -> [a] -> [a]
filter p xs       = [ x | x <- xs, p x ]

concat           :: [[a]] -> [a]
concat            = foldr (++) []

length           :: [a] -> Int
length            = foldl' (\n _ -> n + 1) 0

(!!)             :: [a] -> Int -> a
xs     !! n | n<0 = error "Prelude.!!: negative index"
[]     !! _       = error "Prelude.!!: index too large"
(x:_)  !! 0       = x
(_:xs) !! n       = xs !! (n-1)

iterate          :: (a -> a) -> a -> [a]
iterate f x       = x : iterate f (f x)

repeat           :: a -> [a]
repeat x          = xs where xs = x:xs

replicate        :: Int -> a -> [a]
replicate n x     = take n (repeat x)

take                :: Int -> [a] -> [a]
take n _  | n <= 0  = []
take _ []           = []
take n (x:xs)       = x : take (n-1) xs

drop                :: Int -> [a] -> [a]
drop n xs | n <= 0  = xs
drop _ []           = []
drop n (_:xs)       = drop (n-1) xs

splitAt               :: Int -> [a] -> ([a], [a])
splitAt n xs | n <= 0 = ([],xs)
splitAt _ []          = ([],[])
splitAt n (x:xs)      = (x:xs',xs'') where (xs',xs'') = splitAt (n-1) xs

takeWhile           :: (a -> Bool) -> [a] -> [a]
takeWhile p []       = []
takeWhile p (x:xs)
     | p x       = x : takeWhile p xs
     | otherwise = []

dropWhile           :: (a -> Bool) -> [a] -> [a]
dropWhile p []       = []
dropWhile p xs@(x:xs')
     | p x       = dropWhile p xs'
     | otherwise = xs

reverse   :: [a] -> [a]
reverse    = foldl (flip (:)) []

and, or   :: [Bool] -> Bool
and        = foldr (&&) True
or         = foldr (||) False

any, all  :: (a -> Bool) -> [a] -> Bool
any p      = or  . map p
all p      = and . map p

elem, notElem    :: Eq a => a -> [a] -> Bool
elem              = any . (==)
notElem           = all . (/=)

zip              :: [a] -> [b] -> [(a,b)]
zip               = zipWith  (\a b -> (a,b))

zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3              = zipWith3 (\a b c -> (a,b,c))

zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith _ _      _        = []

zipWith3                 :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
             = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _          = []

unzip                    :: [(a,b)] -> ([a],[b])
unzip                     = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([], [])

unzip3                   :: [(a,b,c)] -> ([a],[b],[c])
unzip3                    = foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs)) ([],[],[])
