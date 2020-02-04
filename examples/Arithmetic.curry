------------------------------------------------------------------------------
-- Example with arithmetic computations to demonstrate the treatment
-- of external operations. Note that this works with the
-- ICurry interpreter in order to avoid the compilation
-- of the complete Curry prelude.
------------------------------------------------------------------------------

(+) :: Int -> Int -> Int
x + y = (prim_Int_plus $# y) $# x

-- The primitive addition `prim_Int_plus` is treated by the ICurry interpreter.
prim_Int_plus :: Int -> Int -> Int
prim_Int_plus external

(*) :: Int -> Int -> Int
x * y = (prim_Int_mult $# y) $# x

-- The primitive addition `prim_Int_mult` is treated by the ICurry interpreter.
prim_Int_mult :: Int -> Int -> Int
prim_Int_mult external

------------------------------------------------------------------------------
-- Combining arithmetic and non-determinism:
coin :: Int
coin = 0 ? 1

coinCoin = coin + coin

double :: Int -> Int
double x = x + x

doubleCoin = double coin

------------------------------------------------------------------------------
-- Higher-order with arithmetic operations.
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

inc123 :: [Int]
inc123 = normalForm (map (1+) [1,2,3])

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

fac5 :: Int
fac5 = foldr (*) 1 [1,2,3,4,5]

------------------------------------------------------------------------------
