-- Testing infinitely many results

aBool :: Bool
aBool = False ? True

aBoolList :: [Bool]
aBoolList = []
aBoolList = aBool : aBoolList

-- infinitely many values: use option --interactive
main :: [Bool]
main = normalForm aBoolList
