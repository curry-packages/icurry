------------------------------------------------------------------------------
--- This module contains an implementation of a case lifter, i.e.,
--- an operation which lifts all nested cases in a FlatCurry program
--- into new operations.
---
--- NOTE: the new operations contain nonsense types, i.e., this transformation
--- should only be used when the actual function types are irrelevant!
---
--- @author Michael Hanus
--- @version January 2020
------------------------------------------------------------------------------

module FlatCurry.CaseLifting where

import List ( maximum, union )

import FlatCurry.Goodies ( allVars )
import FlatCurry.Types

------------------------------------------------------------------------------
--- Options for case lifting.
data LiftOptions = LiftOptions
  { caseFun :: QName -- name of possibly case function
  }

defaultOpts :: LiftOptions
defaultOpts = LiftOptions ("","")

-- Add suffix to case function
add2caseFun :: LiftOptions -> String -> LiftOptions
add2caseFun opts suff =
  let (mn,fn) = caseFun opts
  in opts { caseFun = (mn, fn ++ suff) }

------------------------------------------------------------------------------

--- Lift all nested cases in a FlatCurry program.
liftProg :: LiftOptions -> Prog -> Prog
liftProg opts (Prog mn imps types funs ops) =
  Prog mn imps types (concatMap (liftFun opts) funs) ops

--- Lift all nested cases in a FlatCurry function.
liftFun :: LiftOptions -> FuncDecl -> [FuncDecl]
liftFun opts (Func fn ar vis texp rule) =
  let (nrule, nfs) = liftRule opts { caseFun = fn } rule
  in Func fn ar vis texp nrule : nfs


liftRule :: LiftOptions -> Rule -> (Rule, [FuncDecl])
liftRule _ (External n) = (External n, [])
liftRule opts (Rule args rhs) =
  let (nrhs, nfs) = liftExp opts False rhs
  in (Rule args nrhs, nfs)

liftExp :: LiftOptions -> Bool -> Expr -> (Expr, [FuncDecl])
liftExp _ _ (Var v) = (Var v, [])
liftExp _ _ (Lit l) = (Lit l, [])
liftExp opts _ (Comb ct qn es) =
  let (nes,nfs) = unzip (map (\ (n,e) -> liftExpArg opts n e) (zip [1..] es))
  in (Comb ct qn nes, concat nfs)

liftExp opts lft exp@(Case ct e brs) = case e of
  Var _ -> liftCaseExp lft
  _     -> liftCaseArg
 where
  liftCaseExp False =
    let (ne, nefs) = liftExpArg opts 0 e
        (nbrs, nfs) = unzip (map (liftBranch opts) (zip [1..] brs))
    in (Case ct ne nbrs, nefs ++ concat nfs)
  -- lift case expression by creating new function call:
  liftCaseExp True =
    let vs       = unboundVars exp
        cfn      = caseFun (add2caseFun opts "$CASE")
        noneType = TCons ("Prelude","None") []
        caseFunc = Func cfn (length vs) Private noneType (Rule vs exp)
    in (Comb FuncCall cfn (map Var vs), liftFun opts caseFunc)

  -- lift case with complex (non-variable) case argument:
  liftCaseArg =
    let (ne, nefs) = liftExpArg opts 0 e
        casevar    = maximum (0 : allVars exp) + 1
        vs         = unionMap unboundVarsInBranch brs
        cfn        = caseFun (add2caseFun opts "$COMPLEXCASE")
        noneType   = TCons ("Prelude","None") []
        caseFunc   = Func cfn (length vs + 1) Private noneType
                          (Rule (vs ++ [casevar]) (Case ct (Var casevar) brs))
    in (Comb FuncCall cfn (map Var vs ++ [ne]), nefs ++ liftFun opts caseFunc)

liftExp opts False (Let bs e) =
  let (nes,nfs1) = unzip (map (\ (n,be) -> liftExpArg opts n be)
                         (zip [1..] (map snd bs)))
      (ne,nfs2)  = liftExpArg opts 0 e
  in (Let (zip (map fst bs) nes) ne, concat nfs1 ++ nfs2)
-- lift let expression by creating new function call:
liftExp opts True exp@(Let _ _) =
  let vs       = unboundVars exp
      cfn      = caseFun (add2caseFun opts "$LET")
      noneType = TCons ("Prelude","None") []
      letFunc  = Func cfn (length vs) Private noneType (Rule vs exp)
  in (Comb FuncCall cfn (map Var vs), liftFun opts letFunc)

liftExp opts _ (Free vs e) =
  let (ne, nfs) = liftExp opts True e
  in (Free vs ne, nfs)
liftExp opts _ (Or e1 e2) =
  let (ne1, nfs1) = liftExpArg opts 1 e1
      (ne2, nfs2) = liftExpArg opts 2 e2
  in (Or ne1 ne2, nfs1 ++ nfs2)
liftExp opts lft (Typed e te) =
  let (ne, nfs) = liftExp opts lft e
  in (Typed ne te, nfs)

-- Lift an argument of an expression so that the argument number
-- is added to the case function in order to obtain unique names.
liftExpArg :: LiftOptions -> Int -> Expr -> (Expr, [FuncDecl])
liftExpArg opts argnum = liftExp (add2caseFun opts ('_' : show argnum)) True

liftBranch :: LiftOptions -> (Int,BranchExpr) -> (BranchExpr, [FuncDecl])
liftBranch opts (bnum, Branch pat e) =
  let (ne,nfs) = liftExpArg opts bnum e
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
