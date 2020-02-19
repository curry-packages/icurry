------------------------------------------------------------------------------
--- This module contains an implementation of a case lifter, i.e.,
--- an operation which lifts all nested cases (and also nested lets)
--- in a FlatCurry program into new operations.
---
--- NOTE: the new operations contain nonsense types, i.e., this transformation
--- should only be used if the actual function types are irrelevant!
---
--- @author Michael Hanus
--- @version February 2020
------------------------------------------------------------------------------

module FlatCurry.CaseLifting where

import List ( maximum, union )

import FlatCurry.Goodies ( allVars )
import FlatCurry.Types

------------------------------------------------------------------------------
--- Options for case/let/free lifting.
data LiftOptions = LiftOptions
  { liftCase :: Bool -- lift nested cases?
  , liftCArg :: Bool -- lift non-variable case arguments?
  , currFun  :: QName -- name of current function to be lifted (internally used)
  }

--- Default options for lifting all nested case/let/free expressions.
defaultLiftOpts :: LiftOptions
defaultLiftOpts = LiftOptions True True ("","")

--- Default options for lifting no nested case/let/free expression.
defaultNoLiftOpts :: LiftOptions
defaultNoLiftOpts = LiftOptions False False ("","")

-- Add suffix to case function
addSuffix2Fun :: LiftOptions -> String -> LiftOptions
addSuffix2Fun opts suff =
  let (mn,fn) = currFun opts
  in opts { currFun = (mn, fn ++ suff) }

------------------------------------------------------------------------------

--- Lift nested cases/lets/free in a FlatCurry program (w.r.t. options).
liftProg :: LiftOptions -> Prog -> Prog
liftProg opts (Prog mn imps types funs ops) =
  Prog mn imps types (concatMap (liftFun opts) funs) ops

--- Lift nested cases/lets/free in a FlatCurry function (w.r.t. options).
liftFun :: LiftOptions -> FuncDecl -> [FuncDecl]
liftFun opts (Func fn ar vis texp rule) =
  let (nrule, nfs) = liftRule opts { currFun = fn } rule
  in Func fn ar vis texp nrule : nfs


liftRule :: LiftOptions -> Rule -> (Rule, [FuncDecl])
liftRule _ (External n) = (External n, [])
liftRule opts (Rule args rhs) =
  let (nrhs, nfs) = liftExp opts False rhs
  in (Rule args nrhs, nfs)

-- Lift nested cases/lets/free in expressions.
-- If the second argument is `True`, we are inside an expression where
-- lifting is necessary (e.g., in arguments of function calls).
liftExp :: LiftOptions -> Bool -> Expr -> (Expr, [FuncDecl])
liftExp _ _ (Var v) = (Var v, [])
liftExp _ _ (Lit l) = (Lit l, [])
liftExp opts _ (Comb ct qn es) =
  let (nes,nfs) = unzip (map (\ (n,e) -> liftExpArg opts True n e)
                             (zip [1..] es))
  in (Comb ct qn nes, concat nfs)

liftExp opts nested exp@(Case ct e brs) = case e of
  Var _ -> liftCaseExp
  _     -> if liftCArg opts then liftCaseArg else liftCaseExp
 where
  liftCaseExp =
    if nested -- lift case expression by creating new function
      then let vs       = unboundVars exp
               cfn      = currFun (addSuffix2Fun opts "$CASE")
               noneType = TCons ("Prelude","None") []
               caseFunc = Func cfn (length vs) Private noneType (Rule vs exp)
           in (Comb FuncCall cfn (map Var vs), liftFun opts caseFunc)
      else let (ne, nefs) = liftExpArg opts True 0 e
               (nbrs, nfs) = unzip (map (liftBranch opts) (zip [1..] brs))
           in (Case ct ne nbrs, nefs ++ concat nfs)

  -- lift case with complex (non-variable) case argument:
  liftCaseArg =
    let (ne, nefs) = liftExpArg opts True 0 e
        casevar    = maximum (0 : allVars exp) + 1
        vs         = unionMap unboundVarsInBranch brs
        cfn        = currFun (addSuffix2Fun opts "$COMPLEXCASE")
        noneType   = TCons ("Prelude","None") []
        caseFunc   = Func cfn (length vs + 1) Private noneType
                          (Rule (vs ++ [casevar]) (Case ct (Var casevar) brs))
    in (Comb FuncCall cfn (map Var vs ++ [ne]), nefs ++ liftFun opts caseFunc)

liftExp opts nested exp@(Let bs e)
 | nested -- lift nested let expressions by creating new function
 = let vs       = unboundVars exp
       cfn      = currFun (addSuffix2Fun opts "$LET")
       noneType = TCons ("Prelude","None") []
       letFunc  = Func cfn (length vs) Private noneType (Rule vs exp)
   in (Comb FuncCall cfn (map Var vs), liftFun opts letFunc)
 | otherwise
 = let (nes,nfs1) = unzip (map (\ (n,be) -> liftExpArg opts True n be)
                          (zip [1..] (map snd bs)))
       (ne,nfs2)  = liftExpArg opts True 0 e
   in (Let (zip (map fst bs) nes) ne, concat nfs1 ++ nfs2)

liftExp opts nested exp@(Free vs e)
 | nested -- lift nested free declarations by creating new function
 = let fvs      = unboundVars exp
       cfn      = currFun (addSuffix2Fun opts "$FREE")
       noneType = TCons ("Prelude","None") []
       freeFunc = Func cfn (length fvs) Private noneType (Rule fvs exp)
   in (Comb FuncCall cfn (map Var fvs), liftFun opts freeFunc)
 | otherwise
 = let (ne, nfs) = liftExp opts True e
   in (Free vs ne, nfs)

liftExp opts _ (Or e1 e2) =
  let (ne1, nfs1) = liftExpArg opts True 1 e1
      (ne2, nfs2) = liftExpArg opts True 2 e2
  in (Or ne1 ne2, nfs1 ++ nfs2)
liftExp opts nested (Typed e te) =
  let (ne, nfs) = liftExp opts nested e
  in (Typed ne te, nfs)

-- Lift an argument of an expression so that the argument number
-- is added to the case function in order to obtain unique names.
liftExpArg :: LiftOptions -> Bool -> Int -> Expr -> (Expr, [FuncDecl])
liftExpArg opts nested argnum =
  liftExp (addSuffix2Fun opts ('_' : show argnum)) nested

liftBranch :: LiftOptions -> (Int,BranchExpr) -> (BranchExpr, [FuncDecl])
liftBranch opts (bnum, Branch pat e) =
  let (ne,nfs) = liftExpArg opts (liftCase opts) bnum e
  in (Branch pat ne, nfs)

--- Find all variables which are not bound in an expression.
unboundVars :: Expr -> [VarIndex]
unboundVars (Var idx)     = [idx]
unboundVars (Lit _)       = []
unboundVars (Comb _ _ es) = unionMap unboundVars es
unboundVars (Or e1 e2)    = union (unboundVars e1) (unboundVars e2)
unboundVars (Typed e _)   = unboundVars e
unboundVars (Free vs e)   = filter (not . flip elem vs) (unboundVars e)
unboundVars (Let bs e) =
  let unbounds = unionMap unboundVars $ e : map snd bs
      bounds   = map fst bs
  in filter (not . flip elem bounds) unbounds
unboundVars (Case _ e bs) =
  union (unboundVars e) (unionMap unboundVarsInBranch bs)

unboundVarsInBranch :: BranchExpr -> [VarIndex]
unboundVarsInBranch (Branch (Pattern _ vs) be) =
  filter (not . flip elem vs) (unboundVars be)
unboundVarsInBranch (Branch (LPattern _) be) = unboundVars be

unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldr union [] . map f
