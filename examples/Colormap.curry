-- graph coloring with non-deterministic functions
-- exploiting the demand-driven search due to lazy evaluation in Curry

-- This is our actual map:
--
-- --------------------------
-- |       |        |       |
-- |       |   L2   |       |
-- |       |        |       |
-- |  L1   |--------|  L4   |
-- |       |        |       |
-- |       |   L3   |       |
-- |       |        |       |
-- --------------------------
--

data Color = Red | Green | Blue

aColor = Red
aColor = Green
aColor = Blue

True  && x = x
False && _ = False

cond True x = x

diff Red   Green = True
diff Red   Blue  = True
diff Green Red   = True
diff Green Blue  = True
diff Blue  Red   = True
diff Blue  Green = True

-- correct coloring where non-deterministic generators are provided
correct l1 l2 l3 l4 =
  cond (diff l1 l2 && diff l1 l3 && diff l2 l3 && diff l2 l4 && diff l3 l4)
       [l1,l2,l3,l4]

main1 = normalForm (correct aColor aColor aColor aColor)

-- correct coloring with free variables (and narrowing on `diff`)
correctFree =
  cond (diff l1 l2 && diff l1 l3 && diff l2 l3 && diff l2 l4 && diff l3 l4)
       [l1,l2,l3,l4]
 where l1,l2,l3,l4 free

main2 = normalForm correctFree

main = main2

