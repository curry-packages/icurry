--- Infer type arguments required to properly know the types of free variables
--- @author Marc Andre Wittorf

module ICurry.InferNeededTypeArgs (
  findNeededImports,
  unifyTypes,
  usedImportsExp
) where

import FlatCurry.Types
import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Goodies
import ICurry.Types
import List (nub, partition)
import Sort (sort, sortBy)

--- Infer type arguments a function needs (is a step in a fixpoint iteration)
--- @param known already known needed arguments
--- @param func  the function
--- @return      the function's needed type arguments known so far
findNeededImportsForFunction :: [NeededMapping]
                             -> AFuncDecl TypeExpr
                             -> NeededMapping
findNeededImportsForFunction known (AFunc name _ _ t r) = (name, (t, neededs))
  where
    useds' = usedImportsRule known r
    bounds = allTVars t
    useds  = nub $ sort useds'
    neededs = filter (flip elem bounds) useds

--- Find all type variables used for free variables in a rule
--- @param ms   the known type requirements
--- @param rule the rule
--- @return     the required type arguments
usedImportsRule :: [NeededMapping] -> ARule TypeExpr -> [TVarIndex]
usedImportsRule _ (AExternal _ _) = []
usedImportsRule ms (ARule _ _ e)  = usedImportsExp ms e

--- Find all type variables used for free variables in an expression
--- @param ms   the known type requirements
--- @param expr the expression
--- @return     the required type arguments
usedImportsExp :: [NeededMapping] -> AExpr TypeExpr -> [TVarIndex]
usedImportsExp _  (AVar _ _) = []
usedImportsExp _  (ALit _ _) = []
usedImportsExp ms (ALet _ binds e) = concatMap (usedImportsExp ms) $
                                               e : map snd binds
usedImportsExp ms (AFree _ vs e) = concatMap (allTVars . snd) vs
                                   ++ usedImportsExp ms e
usedImportsExp ms (AOr _ e1 e2) = concatMap (usedImportsExp ms) [e1, e2]
usedImportsExp ms (ATyped _ e _) = usedImportsExp ms e
usedImportsExp ms (ACase _ _ e bs) = usedImportsExp ms e ++
                                     concatMap (trBranch $
                                                const $
                                                usedImportsExp ms)
                                               bs
usedImportsExp ms (AComb _ ConsCall _ es) = concatMap (usedImportsExp ms) es
usedImportsExp ms (AComb _ (ConsPartCall _) _ es) = concatMap
                                                      (usedImportsExp ms)
                                                      es
usedImportsExp ms (AComb _ FuncCall (n,t) es) = fnCallNeeded ms n t
                                                ++ concatMap
                                                     (usedImportsExp ms)
                                                     es
usedImportsExp ms (AComb _ (FuncPartCall _) (n,t) es) = fnCallNeeded ms n t
                                                        ++ concatMap
                                                             (usedImportsExp ms)
                                                             es

--- Find all type variables used for free variables in a function call
--- @param ms   the known type requirements
--- @param n    the called function's name
--- @param expr the function's type (using the callers notation for type vars)
--- @return     the required type arguments
fnCallNeeded :: [NeededMapping] -> QName -> TypeExpr -> [TVarIndex]
fnCallNeeded ms n t = case lookup n ms of
    Nothing -> []
    Just (t', vs) -> nub $
                     concatMap (allTVars . snd) $
                     filter (\x -> elem (fst x) vs) $
                     unifyTypes t' t

--- Unify two types
--- @param tl1 a type expression
--- @param tl2 another type expression
--- @return    a unifier. tl1's variables will be assigned to tyexps from tl2
unifyTypes :: TypeExpr -> TypeExpr -> [(TVarIndex, TypeExpr)]
unifyTypes tl1 tl2 = case (tl1, tl2) of
    ((ForallType vs t),  _                 ) -> unifyTypes t tl2
    (               _ , (ForallType vs t)  ) -> unifyTypes tl1 t
    ((TVar        idx),  _                 ) -> [(idx, tl2)]
    ((FuncType  t1 t2), (FuncType t1' t2') ) -> concat $
                                                zipWith unifyTypes
                                                        [t1, t2]
                                                        [t1', t2']
    ((TCons     n  ts), (TCons    n'  ts') ) -> 
      if n == n'
         then merge $ zipWith unifyTypes ts ts'
         else if n == ("Prelude", "Apply")
                 then merge $ zipWith unifyTypes (tail ts) ts'
                 else error "Clash: Type constructors don't match"
    _                                        -> error "Clash. Cannot unify"
  where
    merge :: [[(TVarIndex, TypeExpr)]] -> [(TVarIndex, TypeExpr)]
    merge ss = merge' $ concat ss
    merge' :: [(TVarIndex, TypeExpr)] -> [(TVarIndex, TypeExpr)]
    merge' [] = []
    merge' ss@(s:_) = let (sames, others) = partition ((fst s ==) . fst) ss
                      in if all (s ==) sames
                            then head sames : merge' others
                            else error "Ambiguous type variable"

--- Find all type variables in a type expression
--- @param te a type expression
--- @return   a list of all type variables in this type expression
allTVars :: TypeExpr -> [TVarIndex]
allTVars (TVar idx)        = [idx]
allTVars (FuncType t1 t2)  = nub $ sort $ concatMap allTVars [t1, t2]
allTVars (TCons _ ts)      = nub $ sort $ concatMap allTVars ts
allTVars (ForallType vs e) = nub $ sort $ vs ++ allTVars e

--- Infer type arguments a function needs for all functions in a modules
---
--- This function iterates over all functions until it has reached a fixpoint,
--- so it even finds a correct solution for mutually recursive function calls
---
--- @param ms   known needed type arguments for all imported modules
--- @param prog the module
--- @return     needed type argument mappings for all functions in this module
findNeededImports :: [NeededMapping] -> AProg TypeExpr -> [NeededMapping]
findNeededImports ms (AProg _ _ _ fs _) =
    find $
    nmsort $
    map (\(AFunc n _ _ t _) -> (n, (t, [])))
    fs
  where
    find :: [NeededMapping] -> [NeededMapping]
    find nms = let nms' = nmsort $
                          map (canonicalize . 
                               (findNeededImportsForFunction $ nms ++ ms))
                          fs
               in if nms == nms'
                     then nms
                     else find nms'
    nmsort :: [NeededMapping] -> [NeededMapping]
    nmsort = sortBy (\(n,_) (m,_) -> n > m)
    canonicalize :: NeededMapping -> NeededMapping
    canonicalize (n,(t,cms)) = (n, (t, sort cms))
