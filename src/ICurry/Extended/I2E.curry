--- Transform ICurry to Extended ICurry
---
--- - Give each name an additional numerical identifier
--- - Limit the maximum nesting depth
---
--- @author Marc Andre Wittorf

module ICurry.Extended.I2E(
  i2eProg,
  extractDepInfoFromI
) where

import Maybe
import List
import ICurry.Extended.Types
import ICurry.Types

data DepInfo =
  DepInfo
    { typeDInfo :: [IEQName]
    , consDInfo :: [IEQName]
    , funcDInfo :: [IEQName]
    } deriving (Show)

type DepInfos = [(String, DepInfo)] -- for all imported modules

--- Translate an ICurry name to an Extended ICurry name
--- @param accessor the lookup function for names
--- @param di       the information about all dependencies
--- @param qn       the name
--- @return         the name
i2eQN :: (DepInfo -> [IEQName]) -> DepInfos -> IQName -> IEQName
i2eQN accessor di qn@(mn,_) =
  fromJust $ (lookup mn di
              >>= lookup qn . accessor
              >>= \x -> return (qn,x))
            `mplus`
              Just (error $ "Cannot find module: " ++ show qn)

--- Some special types which are not directly used by a source program
specialTypes :: [IEQName]
specialTypes = [
  (("Prelude","Apply"), -1)]

--- Translate an ICurry type name to an Extended ICurry type name
--- @param di the information about all dependencies
--- @param qn the type name
--- @return   the type name
i2eQNType :: DepInfos -> IQName -> IEQName
i2eQNType di qn = case lookup qn specialTypes of
                       Just x  -> (qn,x)
                       Nothing -> i2eQN typeDInfo di qn

--- Translate an ICurry constructor name to an Extended ICurry constructor name
--- @param di the information about all dependencies
--- @param qn the constructor name
--- @return   the constructor name
i2eQNCons :: DepInfos -> IQName -> IEQName
i2eQNCons = i2eQN consDInfo

--- Some special functions
--- these are needed as free variables are translated into generator functions
specialFunctions :: [IEQName]
specialFunctions =
  [ (("Prelude","Int"), -1)
  , (("Prelude","Bool"), -1)
  , (("Prelude","Float"), -1)]

--- Translate an ICurry function name to an Extended ICurry function name
--- @param di the information about all dependencies
--- @param qn the function name
--- @return   the function name
i2eQNFunc :: DepInfos -> IQName -> IEQName
i2eQNFunc di qn = case lookup qn specialFunctions of
                       Just x  -> (qn,x)
                       Nothing -> i2eQN funcDInfo di qn

--- Get all defined names from an ICurry module
--- @param prog the module
--- @return     the module name and the defined names
extractDepInfoFromI :: IProg -> (String, DepInfo)
extractDepInfoFromI (IProg n _ dts fns) = (n, DepInfo ti ci fi)
  where
    (ti,ci) = extractTypeInfosFromI dts
    fi'     = map (\(x, n) -> (x,n+1+maxType)) $ extractFuncInfosFromI fns
    maxType = maximum $ -1 : map snd ti
    fi      = ti ++ fi'

--- Get defined type names and constructor names from ICurry data types
--- @param dts the data types
--- @return    the data type names and constructor names
extractTypeInfosFromI :: [IDataType] -> ([IEQName], [IEQName])
extractTypeInfosFromI = extractTypeInfosFromI' 0
  where
    extractTypeInfosFromI' :: Int -> [IDataType] -> ([IEQName], [IEQName])
    extractTypeInfosFromI'  _     [] = ([],[])
    extractTypeInfosFromI' nt (t:ts) =
      let
          (ti, ci)   = extractTypeInfoFromI nt t
          (ti', ci') = extractTypeInfosFromI' (nt + 1) ts
      in
        (ti:ti', ci ++ ci')

    extractTypeInfoFromI nt  (n, k, _, cs) = ((n,nt), ci)
      where
        ci = zipWith (\i (IConstructor c _ _) -> (c,i)) [0..] cs

--- Get defined function names from ICurry functions
--- @param fns the functions
--- @return    the functions' names
extractFuncInfosFromI :: [IFunction] -> [IEQName]
extractFuncInfosFromI = zipWith efi [0..]
  where
    efi :: Int -> IFunction -> IEQName
    efi i (IFunction n _ _) = (n, i)

--- Transform an ICurry module to an Extended ICurry module
--- @param di   the information about all dependencies
--- @param prog the module
--- @return     the module
i2eProg :: DepInfos -> IProg -> IEProg
i2eProg di' prog@(IProg n is dts fns) =
    IEProg n is (map (i2eDType di) dts) (map (i2eFunc di . fixDeepNests) fns)
  where
    di = extractDepInfoFromI prog:di'

--- Transform an ICurry data type to an Extended ICurry data type
--- @param di the information about all dependencies
--- @param dt the data type
--- @return   the data type
i2eDType :: DepInfos -> IDataType -> IEDataType
i2eDType di (n, k, vs, cs) = (i2eQNType di n, i2eVis k, vs, map (i2eCons di) cs)

--- Transform an ICurry constructor to an Extended ICurry constructor
--- @param di the information about all dependencies
--- @param c  the constructor
--- @return   the constructor
i2eCons :: DepInfos -> IConstructor -> IEConstructor
i2eCons di (IConstructor n k te) =
  IEConstructor (i2eQNCons di n) (i2eVis k) (map (i2eTExp di) te)

--- Transform an ICurry type expression to an Extended ICurry type expression
--- @param di the information about all dependencies
--- @param te the type expression
--- @return   the type expression
i2eTExp :: DepInfos -> ITExpr -> IETExpr
i2eTExp _  (ITVar v)      = IETVar v
i2eTExp di (ITFunc e1 e2) = IETFunc (i2eTExp di e1) (i2eTExp di e2)
i2eTExp di (ITCons c es)  = IETCons (i2eQNType di c) (map (i2eTExp di) es)

--- Transform an ICurry visibility to an Extended ICurry visibility
--- @param di the information about all dependencies
--- @param k  the visibility
--- @return   the visibility
i2eVis :: IVisibility -> IEVisibility
i2eVis ICurry.Types.Public = ICurry.Extended.Types.Public
i2eVis ICurry.Types.Private = ICurry.Extended.Types.Private

--- Transform an ICurry function to an Extended ICurry function
--- @param di the information about all dependencies
--- @param fn the function
--- @return   the function
i2eFunc :: DepInfos -> IFunction -> IEFunction
i2eFunc di (IFunction n k b) = IEFunction (i2eQNFunc di n)
                                          (i2eVis k)
                                          (i2eFuncBody di b)

--- Transform an ICurry function body to an Extended ICurry function body
--- @param di the information about all dependencies
--- @param fb the function body
--- @return   the function body
i2eFuncBody :: DepInfos ->  IFuncBody -> IEFuncBody
i2eFuncBody  _ (IExternal n en) = IEExternal n en
i2eFuncBody di (IFuncBody vs b) = IEFuncBody vs $ i2eBlock di b

--- Transform an ICurry assignment to an Extended ICurry assignment
--- @param di the information about all dependencies
--- @param a  the assignment
--- @return   the assignment
i2eAssign :: DepInfos -> IAssign -> IEAssign
i2eAssign di (v, e) = (v, i2eExp di e)

--- Transform an ICurry block to an Extended ICurry block
--- @param di the information about all dependencies
--- @param b  the block
--- @return   the block
i2eBlock :: DepInfos -> IBlock -> IEBlock
i2eBlock di (ISimpleBlock   as    e) =
  IESimpleBlock (map (i2eAssign di) as) $ i2eExp di e
i2eBlock di (ICaseConsBlock as v bs) =
  IECaseConsBlock (map (i2eAssign di) as) v $ map (i2eCBranch di) bs
i2eBlock di (ICaseLitBlock  as v bs) =
  IECaseLitBlock (map (i2eAssign di) as) v $ map (i2eLBranch di) bs

--- Transform an ICurry cons branch to an Extended ICurry cons branch
--- @param di the information about all dependencies
--- @param b  the branch
--- @return   the branch
i2eCBranch :: DepInfos -> IConsBranch -> IEConsBranch
i2eCBranch di (IConsBranch c vs b) =
  IEConsBranch (i2eQNCons di c) vs $ i2eBlock di b

--- Transform an ICurry literal branch to an Extended ICurry literal branch
--- @param di the information about all dependencies
--- @param b  the branch
--- @return   the branch
i2eLBranch :: DepInfos -> ILitBranch -> IELitBranch
i2eLBranch di (ILitBranch l b) = IELitBranch l $ i2eBlock di b

--- Transform an ICurry expression to an Extended ICurry expression
--- @param di the information about all dependencies
--- @param e  the expression
--- @return   the expression
i2eExp :: DepInfos -> IExpr -> IEExpr
i2eExp  _ (IVar v)      = IEVar v
i2eExp  _ (ILit l)      = IELit l
i2eExp di (IFCall n es) = IEFCall (i2eQNFunc di n) $ map (i2eExp di) es
i2eExp di (ICCall n es) = IECCall (i2eQNCons di n) $ map (i2eExp di) es
i2eExp di (IOr es)      = IEOr $ map (i2eExp di) es

--- Maximum nesting depth
---
--- This is intentionally low, as every ICurry level results in multiple levels
--- in the target language
maxDepth = 4

--- Limit nesting depth by breaking deep expressions into new variables
--- @param fn the function
--- @return   the nest-limited function
fixDeepNests :: IFunction -> IFunction
fixDeepNests (IFunction n k b) = IFunction n k $ fdnBody b
  where
    fdnBody :: IFuncBody -> IFuncBody
    fdnBody e@(IExternal _ _) = e
    fdnBody (IFuncBody vs b)  = IFuncBody vs $ fdnBlock 0 vs b

    fdnBlock :: Int -> [IVarIndex] -> IBlock -> IBlock
    fdnBlock i vs (ISimpleBlock as e) =
      let as'       = fdnAssigns i (vs ++ map fst as) as
          (as'',e') = fdnExp i (vs ++ map fst as ++ map fst as') e
      in ISimpleBlock (as'++as'') e'
    fdnBlock i vs (ICaseLitBlock as v bs) =
      let as' = fdnAssigns i (vs ++ map fst as) as
      in ICaseLitBlock as' v $ fdnLBs i (vs ++ map fst as') bs
    fdnBlock i vs (ICaseConsBlock as v bs) =
      let as' = fdnAssigns i (vs ++ map fst as) as
      in ICaseConsBlock as' v $ fdnCBs i (vs ++ map fst as') bs

    fdnAssigns :: Int -> [IVarIndex] -> [IAssign] -> [IAssign]
    fdnAssigns i vs as = uncurry (++) $ chain (fdnAssign i) vs as

    fdnAssign :: Int -> [IVarIndex] -> IAssign -> ([IAssign], IAssign)
    fdnAssign i vs (v,e) = let (as,e') = fdnExp i vs e in (as, (v, e'))

    fdnLBs :: Int -> [IVarIndex] -> [ILitBranch] -> [ILitBranch]
    fdnLBs i vs = map (fdnLB i vs)

    fdnCBs :: Int -> [IVarIndex] -> [IConsBranch] -> [IConsBranch]
    fdnCBs i vs = map (fdnCB i vs)

    fdnLB :: Int -> [IVarIndex] -> ILitBranch -> ILitBranch
    fdnLB i vs (ILitBranch c b) = ILitBranch c $ fdnBlock (i+1) vs b

    fdnCB :: Int -> [IVarIndex] -> IConsBranch -> IConsBranch
    fdnCB i vs (IConsBranch c lvs b) =
      IConsBranch c lvs $ fdnBlock (i+1) (vs ++ lvs) b

    fdnExp :: Int -> [IVarIndex] -> IExpr -> ([IAssign], IExpr)
    fdnExp _  _ e@(IVar v)    = ([], e)
    fdnExp _  _ e@(ILit l)    = ([], e)
    fdnExp i vs (IFCall n es) = fdnExps i vs (IFCall n) es
    fdnExp i vs (ICCall n es) = fdnExps i vs (ICCall n) es
    fdnExp i vs (IOr es)      = fdnExps i vs  IOr       es

    fdnExps :: Int
            -> [IVarIndex]
            -> ([IExpr] -> IExpr)
            -> [IExpr]
            -> ([IAssign], IExpr)
    fdnExps i vs c es
      | i <= maxDepth =
        let
          (as', es') = mapFdnExps (i+1) vs es
        in
          (as', c es')
      | otherwise     =
        let
          v = nextFree vs
          (as',es') = mapFdnExps 0 (v:vs) es
        in
          (as' ++ [(v,c es')], IVar v)

    chain :: ([a] -> b -> ([(a,c)],d)) -> [a] -> [b] -> ([(a,c)],[d])
    chain f  _     [] = ([], [])
    chain f vs (x:xs) =
      let
        (as' , x'  ) = f vs x
        (as'', xs')  = chain f (vs ++ map fst as') xs
      in
        (as' ++ as'', x':xs')

    mapFdnExps :: Int -> [IVarIndex] -> [IExpr] -> ([IAssign], [IExpr])
    mapFdnExps i = chain (fdnExp i)

    nextFree :: [IVarIndex] -> IVarIndex
    nextFree vs = 1 + maximum (-1:vs)
