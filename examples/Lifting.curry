-- Examples to test lifting

-- Without option --nolifting, a new auxiliary function is generated
zip               :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip (_:_)  []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

id :: a -> a
id x = x

-- Without option --nolifting, a new auxiliary function is generated
-- for the non-variable case argument.
complexCase :: Bool -> Bool
complexCase x = case id x of True  -> True
                             False -> False

-- The inner case expression must be lifted
head :: [a] -> a
head xs = id (case xs of x:_ -> x)

-- The inner let declaration must be lifted
ones :: [Int]
ones = id (let xs = 1 : xs in xs)

-- The inner free declaration must be lifted
freevar :: a
freevar = id (let x free in x)

-- The case need not be lifted (although the current ICurry compiler does it)
letCase :: [a] -> Bool
letCase xs = let z = id False
             in case xs of []   -> z
                           _:ys -> letCase ys

freeCase :: [a] -> b
freeCase xs = let z free
              in case xs of []   -> z
                            _:ys -> freeCase ys
