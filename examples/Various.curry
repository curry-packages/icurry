-- Various simple functions for testing.

id x = x

not False = True
not True  = False

notBool = not aBool

fnot = not x  where x free

-- the classical example from the KiCS2 paper:
xor False y = y
xor True  y = not y

xorSelf x = xor x x

aBool = False ? True

xorSelfBool = xorSelf aBool


-- infinite list of 1,2,1,2,...
oneTwo :: [Int]
oneTwo = let x = 1 : y
             y = 2 : x
         in x

head (x:_) = x

headOneTwo = head oneTwo


-- computing permutations
ndinsert x xs     = x:xs
ndinsert x (y:ys) = y : ndinsert x ys

perm []     = []
perm (x:xs) = ndinsert x (perm xs)

perm123 :: [Int]
perm123 = normalForm (perm [1,2,3])

