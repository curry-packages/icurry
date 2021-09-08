-- This example demonstrates the effect of pull tabbing:
-- If a choice occurs deeply in an expression, pull tabbing
-- moves the choice to the top and duplicates the spine
-- before splitting the computations into two tasks.
--
-- To visualize it, executae
--
--     > icurry -g -m main PullTabbing

not :: Bool -> Bool
not False = True
not True  = False

solve :: Bool -> Bool
solve True = True

aBool :: Bool
aBool = False ? True

main = not (not (not (solve aBool)))
