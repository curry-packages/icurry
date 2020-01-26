-- Testing infinitely many results

aBool = False
aBool = True

aBoolList = []
aBoolList = aBool : aBoolList

main = normalForm aBoolList
