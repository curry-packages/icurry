-- Concatenating two lists:
-- (predefined as `++' in the standard prelude)

append :: [a] -> [a] -> [a]
append []     x  = x
append (x:xs) ys = x : append xs ys


-- Reverse the order of elements in a list:

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]


goal1 :: [Int]
goal1 = append [1,2] [3,4]

goal2 :: [Int]
goal2 = rev [1,2,3,4]

-- end of program
