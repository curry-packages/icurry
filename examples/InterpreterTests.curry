------------------------------------------------------------------------------
--- Some tests for the ICurry interpreter.
------------------------------------------------------------------------------

module InterpreterTests
 where

import Test.Prop

import ICurry.Types
import ICurry.Interpreter

------------------------------------------------------------------------------
-- Goodies to support easier construction of ICurry function declarations.

-- A public ICurry function.
iFunction :: String -> IArity -> [Int] -> IFuncBody -> IFunction
iFunction f ar dmd body = IFunction (siq f) ar Public dmd body

-- An ICurry block without free variable declarations so that
-- only the indices of the local variables are provided.
iBlock :: [IVarIndex] -> [IAssign] -> IStatement -> IBlock
iBlock ivs = IBlock (map IVarDecl ivs)

iFCall :: String -> [IExpr] -> IExpr
iFCall n = IFCall (siq n)

iCCall :: String -> [IExpr] -> IExpr
iCCall n = ICCall (siq n)

iFPCall :: String -> Int -> [IExpr] -> IExpr
iFPCall n = IFPCall (siq n)

iCPCall :: String -> Int -> [IExpr] -> IExpr
iCPCall n = ICPCall (siq n)

iConsBranch :: String -> IBlock -> IConsBranch
iConsBranch n = IConsBranch (siq n)

icurryList :: [IExpr] -> IExpr
icurryList []     = ICCall (siq "[]") []
icurryList (x:xs) = ICCall (siq ":")  [x, icurryList xs]

-- A simple right-hand side just returning an expression.
simpleRHS :: IExpr -> IBlock
simpleRHS e = IBlock [] [] (IReturn e)

-- Translate a string into a simple ICurry name without qualified module
-- and index.
siq :: String -> IQName
siq s = ("",s,0)

------------------------------------------------------------------------------
-- Example ICurry programs:

-- coin = 1 ? 2
funCoin :: IFunction
funCoin = iFunction "coin" 0 [] $ IFuncBody $
  simpleRHS (IOr (ILit (IInt 1)) (ILit (IInt 2)))

-- id x = x
funId :: IFunction
funId = iFunction "id" 1 [] $ IFuncBody $
  iBlock [1] [IVarAssign 1 (IVarAccess 0 [0])] (IReturn (IVar 1))

-- idTrue = id True
funIdTrue :: IFunction
funIdTrue = iFunction "idTrue" 1 [] $ IFuncBody $
  simpleRHS (iFCall "id" [iCCall "True" []])

funHead :: IFunction
funHead = iFunction "head" 1 [0] $ IFuncBody $
  iBlock [1] [IVarAssign 1 (IVarAccess 0 [0])]
    (ICaseCons 1
       [iConsBranch "[]" (IBlock [] [] IExempt),
        iConsBranch ":"  (simpleRHS (IVarAccess 1 [0]))])

funHeadEmpty :: IFunction
funHeadEmpty = iFunction "headempty" 0 [] $ IFuncBody $
  simpleRHS (iFCall "head" [iCCall "[]" []])

funHead1 :: IFunction
funHead1 = iFunction "head1" 0 [] $ IFuncBody $
  IBlock [] []
    (IReturn (iFCall "head" [iCCall ":" [ILit (IInt 1), iCCall "[]" []]]))

funHead12 :: IFunction
funHead12 = iFunction "head12" 0 [] $ IFuncBody $
  IBlock [] []
    (IReturn (iFCall "head" [iCCall ":" [iFCall "coin" [],
                                         iCCall "[]" []]]))

funNot :: IFunction
funNot = iFunction "not" 1 [0] $ IFuncBody $
  iBlock [1] [IVarAssign 1 (IVarAccess 0 [0])]
    (ICaseCons 1
       [iConsBranch "False" (simpleRHS (iCCall "True"  [])),
        iConsBranch "True"  (simpleRHS (iCCall "False" []))])

-- (&&) x y = case { False -> False ; True -> y }
funAnd :: IFunction
funAnd = iFunction "&&" 2 [0] $ IFuncBody $
  iBlock [1,2] [IVarAssign 1 (IVarAccess 0 [0]),
                IVarAssign 2 (IVarAccess 0 [1])]
    (ICaseCons 1
       [iConsBranch "False" (simpleRHS (iCCall "False" [])),
        iConsBranch "True"  (simpleRHS (IVar 2))])

-- xor x y = case { False -> y ; True -> not y }
funXor :: IFunction
funXor = iFunction "xor" 2 [0] $ IFuncBody $
  iBlock [1,2]
         [IVarAssign 1 (IVarAccess 0 [0]),
          IVarAssign 2 (IVarAccess 0 [1])]
     (ICaseCons 1
        [iConsBranch "False" (simpleRHS (IVar 2)),
         iConsBranch "True"  (simpleRHS (iFCall "not" [IVar 2]))])

-- xorSelf x = xor x x
funXorSelf :: IFunction
funXorSelf = iFunction "xorSelf" 1 [] $ IFuncBody $
  iBlock [1] [IVarAssign 1 (IVarAccess 0 [0])]
    (IReturn (iFCall "xor" [IVar 1, IVar 1]))

-- aBool = False ? True
funABool :: IFunction
funABool = iFunction "aBool" 0 [] $ IFuncBody $
  simpleRHS (IOr (iCCall "False" []) (iCCall "True" []))

-- notBool = not aBool
funNotBool :: IFunction
funNotBool = iFunction "notBool" 0 [] $ IFuncBody $
  simpleRHS (iFCall "not" [iFCall "aBool" []])

-- xorSelfBool = xorSelf aBool
funXorSelfBool :: IFunction
funXorSelfBool = iFunction "xorSelfBool" 0 [] $ IFuncBody $
  simpleRHS (iFCall "xorSelf" [iFCall "aBool" []])

-- oneTwo = let { x = 1:y ; y = 2:x } in x
funOneTwo :: IFunction
funOneTwo = iFunction "oneTwo" 0 [] $ IFuncBody $
  iBlock [1,2]
         [IVarAssign 1 (iCCall ":" [iCCall "1" [], IVar 2]),
          IVarAssign 2 (iCCall ":" [iCCall "2" [], IVar 1])
         ,INodeAssign 1 [1] (IVar 2)]
         (IReturn (IVar 1))

-- headOneTwo = head (let { x = 1:y ; y = 2:x } in x)
funHeadOneTwo :: IFunction
funHeadOneTwo = iFunction "headOneTwo" 0 [] $ IFuncBody $
  simpleRHS (iFCall "head" [iFCall "oneTwo" []])

-- apply f x: demands f and returns  (f x)
funApply :: IFunction
funApply = iFunction "apply" 2 [0] (IExternal "apply")

-- seq x y: demands x and returns y
funSeq :: IFunction
funSeq = iFunction "seq" 2 [0] $ IFuncBody $
  simpleRHS (IVarAccess 0 [1])

-- f $! x: demands x and returns (f x)
funDollarBang :: IFunction
funDollarBang = iFunction "$!" 2 [1] (IExternal "$!")

-- normalForm x: demands x and returns the normal form of x
funNormalForm :: IFunction
funNormalForm =
  iFunction "normalForm" 1 [0] (IExternal "normalForm")

-- xorSelfSeqBool = let x = aBool in seq x (xorSelf x)
funXorSelfSeqBool :: IFunction
funXorSelfSeqBool = iFunction "xorSelfSeqBool" 0 [] $ IFuncBody $
  iBlock [1] [IVarAssign 1 (iFCall "aBool" [])]
         (IReturn (iFCall "seq" [IVar 1, iFCall "xorSelf" [IVar 1]]))

-- xorSelfDollarBangBool = xorSelf $! aBool
funXorSelfDollarBangBool :: IFunction
funXorSelfDollarBangBool = iFunction "xorSelfDollarBangBool" 0 [] $
  IFuncBody $
    IBlock [] []
           (IReturn (iFCall "$!" [iFPCall "xorSelf" 1 [], iFCall "aBool" []]))

-- andNotFalse = let x = not False in x && x
funAndNotFalse :: IFunction
funAndNotFalse = iFunction "andNotFalse" 0 [] $ IFuncBody $
  iBlock [1] [IVarAssign 1 (iFCall "not" [iCCall "False" []])]
         (IReturn (iFCall "&&" [IVar 1, IVar 1]))

-- coinList = [coin]
funCoinList :: IFunction
funCoinList = iFunction "coinList" 0 [] $ IFuncBody $
  IBlock [] []
         (IReturn (iFCall "normalForm"
                          [iCCall ":" [iFCall "coin" [], iCCall "[]" []]]))

-- coinCoinList = [coin,coin]
funCoinCoinList :: IFunction
funCoinCoinList = iFunction "coinCoinList" 0 [] $ IFuncBody $
  IBlock [] []
    (IReturn (iFCall "normalForm"
                [iCCall ":" [iFCall "coin" [],
                             iCCall ":" [iFCall "coin" [], iCCall "[]" []]]]))

------------------------------------------------------------------------------
-- Non-deterministic list insertion:

-- ndinsert x xs = x:xs ? ndinsert1 x xs
-- ndinsert1 x xs = case xs of { [] -> fail ; y:ys -> y : ndinsert x ys
funNDInsert :: IFunction
funNDInsert = iFunction "ndinsert" 0 [] $ IFuncBody $
  IBlock [] []
    (IReturn
       (IOr (iCCall ":" [IVarAccess 0 [0],
                         IVarAccess 0 [1]])
            (iFCall "ndinsert1" [IVarAccess 0 [0],
                                 IVarAccess 0 [1]])))

funNDInsert1 :: IFunction
funNDInsert1 = iFunction "ndinsert1" 1 [1] $ IFuncBody $
  iBlock [1] [IVarAssign 1 (IVarAccess 0 [1])]
    (ICaseCons 1
       [iConsBranch "[]" (IBlock [] [] IExempt),
        iConsBranch ":"  
          (IBlock [] []
             (IReturn
                (iCCall ":" [IVarAccess 1 [0],
                             iFCall "ndinsert" [IVarAccess 0 [0],
                                                IVarAccess 1 [1]]])))])

-- insert123 = normalForm (ndinsert 1 [2,3])
funInsert123 :: IFunction
funInsert123 = iFunction "insert123" 0 [] $ IFuncBody $
  IBlock [] []
    (IReturn
       (iFCall "normalForm"
          [iFCall "ndinsert"
             [ILit (IInt 1),
              icurryList (map (ILit . IInt) [2,3])]]))

-- perm xs = case xs of { [] -> [] ; y:ys -> ndinsert y (perm ys)
funPerm :: IFunction
funPerm = iFunction "perm" 1 [0] $ IFuncBody $
  iBlock [1] [IVarAssign 1 (IVarAccess 0 [0])]
    (ICaseCons 1
       [iConsBranch "[]" (simpleRHS (iCCall "[]" [])),
        iConsBranch ":"
          (IBlock [] []
             (IReturn
                (iFCall "ndinsert" [IVarAccess 1 [0],
                                    iFCall "perm" [IVarAccess 1 [1]]])))])

-- perm123 = normalForm (perm [1,2,3])
funPerm123 :: IFunction
funPerm123 = iFunction "perm123" 0 [] $ IFuncBody $
  IBlock [] []
    (IReturn (iFCall "normalForm"
                [iFCall "perm" [icurryList (map (ILit . IInt) [1,2,3])]]))

------------------------------------------------------------------------------
-- List of all ICurry function declarations.
allFuns :: [IFunction]
allFuns =
  [ funCoin, funHead, funHeadEmpty, funHead1, funHead12
  , funNot,funAnd, funNotBool, funXor,funXorSelf,funABool,funXorSelfBool
  , funOneTwo, funHeadOneTwo
  , funApply, funDollarBang, funSeq, funNormalForm
  , funXorSelfSeqBool, funXorSelfDollarBangBool
  , funAndNotFalse, funCoinList, funCoinCoinList
  , funNDInsert, funNDInsert1, funPerm, funInsert123, funPerm123
  , funId, funIdTrue
  ]

exampleProg :: IProg
exampleProg = IProg "Example" [] [] allFuns

------------------------------------------------------------------------------
-- Some tests:

quiet :: IOptions
quiet = defOpts

std :: IOptions
std = defOpts { verbosity = 2, withGraph = True, waitTime = 1 }

stdint :: IOptions
stdint = std { interactive = True }

details :: IOptions
details = stdint { verbosity = 4 }

m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14 :: IOptions -> IO ()
m1 o = execIProg o exampleProg "coin"
m2 o = execIProg o exampleProg "headempty"
m3 o = execIProg o exampleProg "head1"
m4 o = execIProg o exampleProg "head12"
m5 o = execIProg o exampleProg "xorSelfBool"
m6 o = execIProg o exampleProg "xorSelfDollarBangBool"
m7 o = execIProg o exampleProg "xorSelfSeqBool"
m8 o = execIProg o exampleProg "andNotFalse"
m9 o = execIProg o exampleProg "headOneTwo"
m10 o = execIProg o exampleProg "coinList"
m11 o = execIProg o exampleProg "coinCoinList"
m12 o = execIProg o exampleProg "perm123"
m13 o = execIProg o exampleProg "idTrue"
m14 o = execIProg o exampleProg "notBool"

------------------------------------------------------------------------------
-- Testing with CurryCheck.

-- Like evalFun but returns results non-deterministically in order to
-- abstract from the evaluation strategy.
evalFunND :: IProg -> String -> String
evalFunND prog fs = anyOf (evalFun prog fs)

testCoin :: Prop
testCoin = evalFunND exampleProg "coin" <~> ("1" ? "2")

testHeadEmpty :: Prop
testHeadEmpty = failing (evalFunND exampleProg "headempty")

testHead1 :: Prop
testHead1 = evalFunND exampleProg "head1" <~> "1"

testHead12 :: Prop
testHead12 = evalFunND exampleProg "head12" <~> ("1" ? "2")

testHeadOneTwo :: Prop
testHeadOneTwo = evalFunND exampleProg "headOneTwo" <~> "1"

testXorSelfBool :: Prop
testXorSelfBool = evalFunND exampleProg "xorSelfBool" <~> "False"

testXorSelfDollarBangBool :: Prop
testXorSelfDollarBangBool =
  evalFunND exampleProg "xorSelfDollarBangBool" <~> "False"

testXorSelfSeqBool :: Prop
testXorSelfSeqBool = evalFunND exampleProg "xorSelfSeqBool" <~> "False"

testAndNotFalse :: Prop
testAndNotFalse = evalFunND exampleProg "andNotFalse" <~> "True"

testNotBool :: Prop
testNotBool = evalFunND exampleProg "notBool" <~> ("True" ? "False")

testIdTrue :: Prop
testIdTrue = evalFunND exampleProg "idTrue" <~> "True"

testCoinList :: Prop
testCoinList = evalFunND exampleProg "coinList" <~> ("(: 1 [])" ? "(: 2 [])")

testCoinCoinList :: Prop
testCoinCoinList =
  evalFunND exampleProg "coinCoinList" <~>
  ("(: 1 (: 1 []))" ? "(: 2 (: 1 []))" ? "(: 1 (: 2 []))" ? "(: 2 (: 2 []))")

testPerm123 :: Prop
testPerm123 =
  evalFunND exampleProg "perm123" <~>
  ("(: 1 (: 2 (: 3 [])))" ? "(: 1 (: 3 (: 2 [])))" ? "(: 2 (: 1 (: 3 [])))" ?
   "(: 2 (: 3 (: 1 [])))" ? "(: 3 (: 1 (: 2 [])))" ? "(: 3 (: 2 (: 1 [])))")

------------------------------------------------------------------------------
