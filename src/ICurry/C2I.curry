--- Compile Typed FlatCurry to ICurry
--- @author Marc Andre Wittorf

module ICurry.C2I(
  flatToI
) where

import FlatCurry.Types
import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Goodies
import ICurry.Types
import ICurry.InferNeededTypeArgs
import List (maximum)
import Maybe
import ICurry.Pretty

--- Transform an annotated-flatcurry program into an icurry program
--- @param ms   the required type arguments
--- @param prog the FlatCurry module
--- @return     the ICurry module
flatToI :: [NeededMapping] -> AProg TypeExpr -> IProg
flatToI ms = trProg $ fti ms

--- Transform an annotated-flatcurry program into an icurry program
--- @param ms        list of required type arguments
--- @param modname   the module name
--- @param imports   names of imported modules
--- @param typedecls type declarations in this module
--- @param functions function declarations in this module
--- @param opdecls   operator declarations in this module
--- @return          an ICurry module
fti :: [NeededMapping]
    -> String
    -> [String]
    -> [TypeDecl]
    -> [AFuncDecl TypeExpr]
    -> [OpDecl]
    -> IProg
fti ms modname imports typedecls functions _ =
    IProg modname
          imports
          (concatMap typeToI typedecls)
          (concatMap (funcToI ms) functions ++ extraFunctions)
  where
    extraFunctions = if modname == "Prelude"
                        then [IFunction ("Prelude", "unshare")
                                        ICurry.Types.Public $
                                        IExternal 1 "Prelude.unshare"]
                        else []

--- Transform an annotated-flatcurry type into an icurry type
--- @param typedecl a type declaration
--- @return         the ICurry type declaration
typeToI :: TypeDecl -> [IDataType]
typeToI = trType (\a b c d -> [t2i a b c d])  (\_ _ _ _ -> [])

--- Transform an annotated-flatcurry type into an icurry type
--- @param name type name
--- @param k    usabe outside of this module?
--- @param vs   type variables this type is parameterized over
--- @param cs   constructors
--- @return     the ICurry type
t2i :: QName
    -> Visibility
    -> [TVarIndex]
    -> [ConsDecl]
    -> IDataType
t2i name k vs cs = (name, v2i k, vs, map (trCons c2i) cs)

--- Transform an annotated-flatcurry constructor into an icurry constructor
--- @param name  constructor name
--- @param arity constructor's arity
--- @param k     usable outside of this module?
--- @param ts    types of arguments
--- @return      the ICurry constructor
c2i :: QName
    -> Int
    -> Visibility
    -> [TypeExpr]
    -> IConstructor
c2i name arity k ts = IConstructor name (v2i k) $ map te2i ts

--- Transform a flatcurry type expression to an icurry type expression
--- @param te the type expression
--- @return   the ICurry type expression
te2i :: TypeExpr -> ITExpr
te2i (TVar v)         = ITVar v
te2i (FuncType e1 e2) = ITFunc (te2i e1) (te2i e2)
te2i (TCons n es)     = ITCons n $ map te2i es
te2i (ForallType _ e) = te2i e

--- Transform an annotated-flatcurry function into an icurry function
--- @param ms list of required type arguments
--- @param fn the annotated-flatcurry function
--- @return   the ICurry function
funcToI :: [NeededMapping]
        -> AFuncDecl TypeExpr
        -> [IFunction]
funcToI ms = trFunc $ f2i' ms

--- Transform an annotated-flatcurry visibility into an icurry visibility
--- @param k the visibility
--- @return  the ICurry visibility
v2i :: Visibility -> IVisibility
v2i FlatCurry.Types.Public = ICurry.Types.Public
v2i FlatCurry.Types.Private = ICurry.Types.Private

--- Transform an annotated-flatcurry function into an icurry function
--- @param ms    list of required type arguments
--- @param name  the function's name
--- @param arity the function's arity
--- @param k     usable outside of this module?
--- @param te    the function's signature
--- @param rule  the function's implementation
--- @return      the ICurry function
f2i' :: [NeededMapping]
     -> QName
     -> Int
     -> Visibility
     -> TypeExpr
     -> ARule TypeExpr
     -> [IFunction]
f2i' _  name arity k _ (AExternal _ ename) =
    [IFunction name (v2i k) $ IExternal arity ename]
f2i' ms name _ k _ (ARule _ vars exp) =
    map intermediateToIFunction $
        f2i ms (nameGenerator name
               ,v2i k
               ,neededTypeVars ++ map fst vars
               ,typeMapping
               ,exp)
  where
    neededTypes = snd $ fromJust $ lookup name ms
    neededTypeVars = newVars (length neededTypes) (map fst vars ++ allVars exp)
    typeMapping = zip neededTypes neededTypeVars

type TypeArg = (TVarIndex, VarIndex)
type IntermediateFlatFunctionInfo =
  (NameGenerator    --required for extending a name (for lambda lifted fns)
  , IVisibility     --usable outside of this module?
  , [VarIndex]      --the function's arguments
  , [TypeArg]       --type arguments this function requires
  , AExpr TypeExpr) --the function's top-level right-hand side expression
type IntermediateIFunctionInfo =
  (NameGenerator    --required for extending a name (for lambda lifted fns)
  , IVisibility     --usable outside of this module?
  , [IVarIndex]     --the function's arguments
  , IBlock)         --the block defining the function's right-hand side
type NameGenerator = (IQName, Bool, Int)

--- Extend a name by appending some (hopefully unique) suffix
--- @param ng a name generator
--- @return   a name generator for this function and one for the new function
extendName :: NameGenerator -> (NameGenerator, NameGenerator)
extendName ((mname,lname), False, n) =
  (((mname,lname), False, n+1), ((mname, "_impl"++lname++'_':show n), True, 1))
extendName ((mname,lname),  True, n) =
  (((mname,lname),  True, n+1), ((mname,          lname++'_':show n), True, 1))

--- Get the current name from a name generator
--- @param ng the name generator
--- @return   a name for the function this generator belongs to
generateOwnName :: NameGenerator -> IQName
generateOwnName (n, _, _) = n

--- Make a name generator from an existing name
---
--- Use extendName before generateOwnName, as
---    generateOwnName (nameGenerator n) == n
--- holds.
---
--- @param n the function's name
--- @return  a name generator
nameGenerator :: IQName -> NameGenerator
nameGenerator n = (n, False, 1)

--- Transform an intermediate function representation into an actual icurry one
--- @param ifi the intermediate function representation
--- @return    the actual icurry function
intermediateToIFunction :: IntermediateIFunctionInfo -> IFunction
intermediateToIFunction (name, k, vars, block) =
  IFunction (generateOwnName name) k $ IFuncBody vars block

--- Transform an annotated-flatcurry function in intermediate representation
--- into one or multiple (through lambda lifting) icurry functions in
--- intermediate representation
---
--- @param ms  list of required type arguments
--- @param ifi the intermediate function representation
--- @return    one or multiple intermediate icurry functions
f2i :: [NeededMapping]
    -> IntermediateFlatFunctionInfo
    -> [IntermediateIFunctionInfo]
f2i ms (ng, k, vars, tm, exp) = let (fn, is, ng') = e2b ms tm ng vars vars exp
                                in (ng', k, vars, fn) : concatMap (f2i ms) is

--- Transform an annotated-flatcurry expression into an icurry block
--- optionally yields more functions through lambda lifting
---
--- @param ms  list of required type arguments
--- @param tm  type arguments the function is given
--- @param ng  generator to determine the function's name
--- @param v   all available variables (arguments and locals)
--- @param fa  function arguments
--- @param tle the expression
--- @return    the block
--- @return    lambda lifted additional functions
--- @return    the name generator with its counters
---            advanced by having used it
e2b :: [NeededMapping]
    -> [TypeArg]
    -> NameGenerator
    -> [VarIndex]
    -> [VarIndex]
    -> AExpr TypeExpr
    -> (IBlock
       ,[IntermediateFlatFunctionInfo]
       ,NameGenerator)

e2b ms tm ng v fargs tle =
    case tle of
      (ATyped _ e _)                                  -> e2b ms tm ng v fargs e
      (ALet _ as e)                                   ->
        let
          vs = map (fst . fst) as
          (es', vs', as', fs', ng') = chainE2i (e2i ms tm)
                                               ng
                                               (v++vs)
                                               fargs
                                               (map snd as)
          (b, fs'', ng'') = e2b ms tm ng' (v++vs++vs') fargs e
          b' = case b of
                    ISimpleBlock   bas be    ->
                      ISimpleBlock   (zip vs es' ++ bas) be
                    ICaseConsBlock bas dv bs ->
                      ICaseConsBlock (zip vs es' ++ bas) dv bs
                    ICaseLitBlock  bas dv bs ->
                      ICaseLitBlock  (zip vs es' ++ bas) dv bs
        in
          (b', fs' ++ fs'', ng'')
      (ACase _ _ e bs@(ABranch (ALPattern _ _) _:_))  ->
        let
          (e', vs', as', fs, ng') = e2i ms tm ng v fargs e
          (dv, vs'', as'') = case e' of
                          (IVar v) -> (v, vs', as')
                          _        -> let nv = newVar (v ++ vs')
                                      in (nv, vs' ++ [nv], as'++[(nv,e')])
          (bs', fs', ng'') = branchesToLitBlocks ms tm ng' (v++vs'') fargs bs
        in
          if dv `elem` fargs
             then (ICaseLitBlock as'' dv bs', fs ++ fs', ng'')
             else simpleBlock
      (ACase _ _ e bs@(ABranch (APattern _ _ _) _:_)) ->
        let
          (e', vs', as', fs, ng') = e2i ms tm ng v fargs e
          (dv, vs'', as'') = case e' of
                              (IVar v) -> (v, vs', as')
                              _        -> let nv = newVar (v ++ vs')
                                          in (nv, vs' ++ [nv], (nv,e'):as')
          (bs', fs', ng'') = branchesToConsBlocks ms tm ng' (v++vs'') fargs bs
        in
          if dv `elem` fargs
             then (ICaseConsBlock as'' dv bs', fs ++ fs', ng'')
             else simpleBlock
      _                                               -> simpleBlock
  where
    simpleBlock = let (e, vs, as, fs, ng') = e2i ms tm ng v fargs tle
                  in (ISimpleBlock as e, fs, ng')

--- Transform annotated-flatcurry branches over literals to icurry branches
--- @param ms list of required type arguments
--- @param tm type arguments the surrounding function is given
--- @param ng name generator for deriving lifted fns
--- @param vs all available variables
--- @param fa only variables given as fn arguments
--- @param bs the flatcurry branches
--- @return   the icurry branches
--- @return   lifted additional fns
--- @return   the used name generator
branchesToLitBlocks :: [NeededMapping]
                    -> [TypeArg]
                    -> NameGenerator
                    -> [VarIndex]
                    -> [VarIndex]
                    -> [ABranchExpr TypeExpr]
                    -> ([ILitBranch]
                       ,[IntermediateFlatFunctionInfo]
                       ,NameGenerator)
branchesToLitBlocks ms tm ng  _ fargs     [] = ([], [], ng)
branchesToLitBlocks ms tm ng vs fargs (b:bs) = (b':bs', fs++fs', ng'')
  where
    (b',  fs,   ng') = branchToLitBlock ms tm ng vs fargs b
    (bs', fs', ng'') = branchesToLitBlocks ms tm ng' vs fargs bs

--- Transform an annotated-flatcurry branch over literals to an icurry branch
--- @param ms list of required type arguments
--- @param tm type arguments the surrounding function is given
--- @param ng name generator for deriving lifted fns
--- @param vs all available variables
--- @param fa only variables given as function arguments
--- @param b  the flatcurry branch
--- @return   the icurry branch
--- @return   lifted additional funcs
--- @return   the used name generator
branchToLitBlock :: [NeededMapping]
                 -> [TypeArg]
                 -> NameGenerator
                 -> [VarIndex]
                 -> [VarIndex]
                 -> ABranchExpr TypeExpr
                 -> (ILitBranch
                    ,[IntermediateFlatFunctionInfo]
                    ,NameGenerator)
branchToLitBlock ms tm ng vs fargs (ABranch (ALPattern _ lit) e) =
    (ILitBranch (l2i lit) block, fs, ng')
  where
    (block, fs, ng') = e2b ms tm ng vs fargs e

--- Transform annotated-flatcurry branches over constructors to icurry branches
--- @param ms list of required type arguments
--- @param tm type arguments the surrounding function is given
--- @param ng name generator for deriving lifted fns
--- @param vs all available variables
--- @param fa only variables given as fn arguments
--- @param bs the flatcurry branches
--- @return   the icurry branches
--- @return   additional lifted fns
--- @return   the used name gen
branchesToConsBlocks :: [NeededMapping]
                     -> [TypeArg]
                     -> NameGenerator
                     -> [VarIndex]
                     -> [VarIndex]
                     -> [ABranchExpr TypeExpr]
                     -> ([IConsBranch]
                        ,[IntermediateFlatFunctionInfo]
                        ,NameGenerator)
branchesToConsBlocks ms tm ng  _ fargs     [] = ([], [], ng)
branchesToConsBlocks ms tm ng vs fargs (b:bs) = (b':bs', fs++fs', ng'')
  where
    (b',  fs,   ng') = branchToConsBlock ms tm ng vs fargs b
    (bs', fs', ng'') = branchesToConsBlocks ms tm ng' vs fargs bs

--- Transform an annotated-flatcurry branch over a constructor to an icurry branch
--- @param ms list of required type arguments
--- @param tm type arguments the surrounding function is given
--- @param ng name generator for deriving lifted fns
--- @param vs all available variables
--- @param fa only variables given as fn arguments
--- @param b  the flatcurry branch
--- @return   the icurry branch
--- @return   additional lifted funcs
--- @return   the used name generator
branchToConsBlock :: [NeededMapping]
                  -> [TypeArg]

                  -> NameGenerator
                  -> [VarIndex]
                  -> [VarIndex]
                  -> ABranchExpr TypeExpr
                  -> (IConsBranch
                     ,[IntermediateFlatFunctionInfo]
                     ,NameGenerator)
branchToConsBlock ms tm ng v fargs (ABranch (APattern _ cons binds) e) =
    (IConsBranch (fst cons) (map fst binds) block, fs, ng')
  where
    (block, fs, ng') = e2b ms tm ng (v ++ map fst binds) fargs e

--- Helper function to chain e2i calls while preserving name generators
--- @param f  the function to chain
--- @param ng this is passed through the chain
--- @param vs this is the start value of an accumulation (via (++))
--- @param fa this is passed verbatim to all f calls
--- @param es this is given to each call
--- @return   each first result of f
--- @return   the accumulated result of vs
--- @return   an accumulation of the third results of f
--- @return   the ng result from the last f call
chainE2i :: (a -> [b]
               -> [g]
               -> c
               -> (d, [b], [e], [f], a))
         -> a
         -> [b]
         -> [g]
         -> [c]
         -> ([d], [b], [e], [f], a)
chainE2i f ng vs     _     [] = ([], [], [], [], ng)
chainE2i f ng vs fargs (e:es) =
  let
    (e',   vs',  as',  fs', ng')  = f ng vs fargs e
    (es'', vs'', as'', fs'', ng'') = chainE2i f ng' (vs++vs') fargs es
  in
    (e':es'', vs'++vs'', as'++as'', fs' ++ fs'', ng'')

--- Transform an annotated-flatcurry expression into an icurry expression
--- @param ms list of required type arguments
--- @param tm type arguments the surrounding function is given
--- @param ng name generator for deriving lifted functions
--- @param vs all available variables
--- @param fa only variables given as function arguments
--- @param e  the flatcurry expression
--- @return   the icurry expression
--- @return   additional local variables introduced
--- @return   bindings for these new local variables
--- @return   lambda lifted additional functions
--- @return   the used (possibly advanced) name generator
e2i :: [NeededMapping]
    -> [TypeArg]
    -> NameGenerator
    -> [VarIndex]
    -> [VarIndex]
    -> AExpr TypeExpr
    -> (IExpr
       ,[VarIndex]
       ,[IAssign]
       ,[IntermediateFlatFunctionInfo]
       ,NameGenerator)
e2i ms tm ng v fargs (AVar _ idx)      = (IVar idx, [], [], [], ng)
e2i ms tm ng v fargs (ALit _ lit)      = (ILit $ l2i lit, [], [], [], ng)
e2i ms tm ng v fargs (ATyped _ e _)    = e2i ms tm ng v fargs e
e2i ms tm ng v fargs (AComb _ t cn es) =
  let
    (es', vs', as', fs, ng') = chainE2i (e2i ms tm) ng v fargs es
    cTyArgs = lookup (fst cn) ms
    tyArgs = case cTyArgs of
                  Nothing      -> []
                  Just (t,tvs) -> let tms = unifyTypes t (snd cn)
                                      needed = map (te2i .
                                                    fromJust .
                                                    flip lookup tms) tvs
                                  in map (tyExpToGenCallUnshare .
                                          replaceTyVars tm) needed
  in
    (combTypeToExpr t (fst cn) (tyArgs ++ es'), vs', as', fs, ng')
e2i ms tm ng v fargs caseE@(ACase ty t e bs) =
  let
    (e', vs', as', fs', ng') = e2i ms tm ng v fargs e
    (nv, vs'', as'') = case e' of
                            IVar idx  -> (idx, vs', [])
                            otherwise -> let nv = newVar (v ++ allVars caseE)
                                         in (nv, vs'++[nv], as' ++ [(nv, e')])
    (ng'', ng''') = extendName ng'
    (f@(fname,_,_,_,_),vs) = lambdaLift ms tm ng''' (ACase ty t (AVar ty nv) bs)
  in
    (IFCall (generateOwnName fname) $ map IVar vs, vs'', as'', fs'++[f], ng'')
e2i ms tm ng v fargs (ALet _ as e) =
  let
    vs = map (fst . fst) as
    (es', vs', as', fs', ng') = chainE2i (e2i ms tm)
                                         ng
                                         (v++vs)
                                         fargs
                                         (map snd as)
    (e'', vs'', as'', fs'', ng'') = e2i ms tm ng' (v++vs++vs') fargs e
  in
    (e'', vs++vs'++vs'', as'++as''++zip vs es', fs'++fs'', ng'')
e2i ms tm ng v fargs (AFree _ vs e) =
  let
    v2g = \(idx, t) -> (idx, tyExpToGenCall $ replaceTyVars tm $ te2i t)
    as'' = map v2g vs
    (e',vs',as',fs',ng'') = e2i ms tm ng (v++(map fst vs)) fargs e
  in
    (e',(map fst vs)++vs',as'++as'',fs', ng'')
e2i ms tm ng v fargs (AOr _ e1 e2) =
  let
    (es', vs', as', fs', ng') = chainE2i (e2i ms tm) ng v fargs [e1, e2]
  in
    (IOr es', vs', as', fs', ng')

--- Transform a flatcurry combination type into an icurry expression
--- @param ct the type of the call
--- @param n  the name of the called function/constructor
--- @param es the arguments
--- @return   the ICurry call
combTypeToExpr :: CombType -> IQName -> [IExpr] -> IExpr
combTypeToExpr FuncCall         = IFCall
combTypeToExpr ConsCall         = ICCall
combTypeToExpr (FuncPartCall _) = IFCall
combTypeToExpr (ConsPartCall _) = ICCall

--- Adjust an type expression to its context
--- This replaces all type variables with those from the surrounding function
--- which represent the corresponding type
--- @param tvs type variables used in the function
--- @param e   a type expression
--- @return    the type expression with all variables replaced
replaceTyVars :: [TypeArg]
              -> ITExpr
              -> ITExpr
replaceTyVars tvs (ITVar v)        = ITVar $ fromJust $ lookup v tvs
replaceTyVars tvs (ITFunc e1 e2)   = ITFunc (replaceTyVars tvs e1)
                                            (replaceTyVars tvs e2)
replaceTyVars tvs (ITCons name es) = ITCons name $ map (replaceTyVars tvs) es

--- Generate some new variables
--- @param n  the number of needed variables
--- @param xs all already used (ie. unavailable) variables
--- @return   some new variables
newVars :: Int
        -> [VarIndex]
        -> [VarIndex]
newVars n xs = take n [maximum (0:xs) + 1 ..]

--- Generate a new variable
--- @param xs all already used (ie. unavailable) variables
--- @return   a new variable
newVar :: [VarIndex]
       -> VarIndex
newVar = head . newVars 1

--- Lift a flatcurry expression into a flatcurry function
--- @param ms list of required type arguments
--- @param tm available type arguments in the surrounding fn
--- @param ng name generator to build a name for the lifted fn
--- @param e  the expression
--- @return   the new function
--- @return   pass these variables from the
--- @return   original function to the lifted fn
lambdaLift :: [NeededMapping]
           -> [TypeArg]
           -> NameGenerator
           -> AExpr TypeExpr
           -> (IntermediateFlatFunctionInfo
              ,[IVarIndex])

lambdaLift ms tm ng e = ((ng
                         , ICurry.Types.Private
                         , news
                         , zip tyArgs news
                         , e')
                        , tyArgVars ++ unbounds)
  where
    allTyVars = usedImportsExp ms e
    tyArgs = filter (flip elem (map fst tm)) allTyVars
    tyArgVars = map (fromJust . flip lookup tm) tyArgs
    unbounds = unboundVars e
    news = newVars (length tyArgs + length unbounds) []
    bounds = filter (not . flip elem unbounds) $ allVars e
    renamer x = fromJust $
                lookup x
                       (zip (unbounds++bounds)
                            [length tyArgs + 1 ..])
    e' = rnmAllVars renamer e

--- Find all variable uses which do not have bindings
---
--- We use 'unbound' instead of 'free' as to not confuse with free variables
---
--- @param e a FlatCurry expression
--- @return  all unbound variables
unboundVars :: AExpr a -> [VarIndex]
unboundVars (AVar _ idx)   = [idx]
unboundVars (ALit _ _) = []
unboundVars (AComb _ _ _ es) = concatMap unboundVars es
unboundVars (AOr _ e1 e2) = concatMap unboundVars [e1,e2]
unboundVars (ATyped _ e _) = unboundVars e
unboundVars (AFree _ vs e) = filter (not . flip elem (map fst vs))
                                    (unboundVars e)
unboundVars (ALet _ as e) =
  let
    unbounds = concatMap unboundVars $ e : map snd as
    bounds = map (fst . fst) as
  in
    filter (not . flip elem bounds) unbounds
unboundVars (ACase _ _ e bs) = concatMap branchUnbounded bs ++ unboundVars e
  where
    branchUnbounded (ABranch (APattern _ _ vs) e') =
      filter (not . flip elem (map fst vs)) (unboundVars e')
    branchUnbounded (ABranch (ALPattern  _  _) e') =
      unboundVars e'

--- Transform a free variable's type expression into appropriate generator calls
---
--- Uses unshare to properly separate choices
---
--- @param te a type expression
--- @return   a generator call
tyExpToGenCallUnshare :: ITExpr -> IExpr
tyExpToGenCallUnshare (ITVar v)     =
  IFCall ("Prelude", "unshare") [IVar v]
tyExpToGenCallUnshare (ITFunc _ _)  =
  IFCall ("Prelude", "error")
         [stringToI "Cannot have generator for functional type"]
tyExpToGenCallUnshare (ITCons c as) =
  IFCall c (map tyExpToGenCall $ as)

--- Transform a free variable's type expression into appropriate generator calls
---
--- does not use unshare:
--- if a generator is passed to another generator, this other generator needs to
--- wrap it in unshare anyways, hence there is no need to unshare it before
--- passing it to the other generator
---
--- @param te a type expression
--- @return   a generator call
tyExpToGenCall :: ITExpr -> IExpr
tyExpToGenCall (ITVar v)     =
  IVar v
tyExpToGenCall (ITFunc _ _)  =
  IFCall ("Prelude", "error")
         [stringToI "Cannot have generator for functional type"]
tyExpToGenCall (ITCons c as) =
  IFCall c (map tyExpToGenCall $ as)

--- Transform a flatcurry literal to an icurry literal
--- @param l a literal
--- @return  the literal
l2i :: Literal -> ILiteral
l2i (Intc   v) = IInt   v
l2i (Floatc v) = IFloat v
l2i (Charc  v) = IChar  v

--- Transform a flatcurry string to an icurry list of characters
--- @param s the string
--- @return  an ICurry char list representing s
stringToI :: String -> IExpr
stringToI []     = ICCall ("Prelude", "[]") []
stringToI (x:xs) = ICCall ("Prelude", ":") [ILit $ IChar x, stringToI xs]
