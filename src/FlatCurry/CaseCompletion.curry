------------------------------------------------------------------------------
--- This module contains an implementation of case completion, i.e.,
--- all case expressions occurring in a FlatCurry program are completed
--- (with calls to `Prelude.failed`) and ordered according to the
--- constructor ordering of the corresponding data definitions.
---
--- @author Michael Hanus
--- @version March 2021
------------------------------------------------------------------------------

module FlatCurry.CaseCompletion where

import Data.List

import FlatCurry.Files
import FlatCurry.Types

------------------------------------------------------------------------------
--- Type to represent algebraic data declarations
--- (pair of type name and list of constructor names and arities).
type DataDecl = (QName, [(QName,Int)])

--- Options for case completion.
data CaseOptions = CaseOptions
  { dataDecls :: [DataDecl]
  }

typeOfConstructor :: CaseOptions -> QName -> DataDecl
typeOfConstructor opts qn =
  let ctypes = filter (\ (_,cs) -> qn `elem` map fst cs) (dataDecls opts)
  in if null ctypes
       then error $ "Type of constructor '" ++ snd qn ++ "' not found!"
       else head ctypes

------------------------------------------------------------------------------

--- Complete all nested cases in a FlatCurry program.
completeProg :: CaseOptions -> Prog -> Prog
completeProg opts (Prog mn imps types funs ops) =
  Prog mn imps types (map (completeFun opts) funs) ops

--- Complete all nested cases in a FlatCurry function.
completeFun :: CaseOptions -> FuncDecl -> FuncDecl
completeFun opts (Func fn ar vis texp rule) =
  Func fn ar vis texp (completeRule opts rule)

completeRule :: CaseOptions -> Rule -> Rule
completeRule _ (External n) = External n
completeRule opts (Rule args rhs) =
  Rule args (completeExp opts rhs)

completeExp :: CaseOptions -> Expr -> Expr
completeExp _ (Var v) = Var v
completeExp _ (Lit l) = Lit l
completeExp opts (Comb ct qn es) =
  Comb ct qn (map (completeExp opts) es)
completeExp opts (Let bs e) =
  Let (zip (map fst bs) (map (completeExp opts . snd) bs)) (completeExp opts e)
completeExp opts (Free vs e) = Free vs (completeExp opts e)
completeExp opts (Or e1 e2) =
  Or (completeExp opts e1) (completeExp opts e2)
completeExp opts (Typed e te) = Typed (completeExp opts e) te
completeExp opts (Case ct e brs) = case brs of
  [] -> Case ct ce []
  Branch (LPattern _)   _ : _ ->  Case ct ce cbrs
  Branch (Pattern cn _) _ : _ ->
    let consdecls = snd (typeOfConstructor opts cn)
        sbrs = map (\c -> maybe (failedBranch c) id
                                (find (isBranchForCons c) cbrs))
                   consdecls
    in Case ct ce sbrs
 where
  isBranchForCons c (Branch pat _) = case pat of Pattern pn _ -> fst c == pn
                                                 _            -> False
  ce   = completeExp opts e
  cbrs = map (\ (Branch pat be) -> Branch pat (completeExp opts be)) brs
  failedBranch (c,ar) = Branch (Pattern c [101 .. 100+ar])
                               (Comb FuncCall ("Prelude","failed") [])

------------------------------------------------------------------------------

--- Get all constructors occurring in case expressions in a FlatCurry program.
allConsProg :: Prog -> [QName]
allConsProg (Prog _ _ _ funs _) = unionMap allConsFun funs

--- AllCons all nested cases in a FlatCurry function.
allConsFun :: FuncDecl -> [QName]
allConsFun (Func _ _ _ _ (External _)) = []
allConsFun (Func _ _ _ _ (Rule _ exp)) = allConsExp exp

allConsExp :: Expr -> [QName]
allConsExp (Var _) = []
allConsExp (Lit _) = []
allConsExp (Comb _ _ es) = unionMap allConsExp es
allConsExp (Case _ e brs) =
  union (allConsExp e) (unionMap allConsBranch brs)
 where
  allConsBranch (Branch (LPattern _)   be) = allConsExp be
  allConsBranch (Branch (Pattern qn _) be) = union [qn] (allConsExp be)
allConsExp (Let bs e) =
  union (allConsExp e) (unionMap (allConsExp . snd) bs)
allConsExp (Free _ e) = allConsExp e
allConsExp (Or e1 e2) = union (allConsExp e1) (allConsExp e2)
allConsExp (Typed e _) = allConsExp e

unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldr union [] . map f

------------------------------------------------------------------------------
--- Get all data types (pairs of type name and list of constructor names
--- and arities) in a given FlatCurry program.
dataDeclsOf :: Prog -> [DataDecl]
dataDeclsOf (Prog _ _ tdecls _ _) = concatMap dataDeclsOfTypeDecl tdecls
 where
  dataDeclsOfTypeDecl (TypeSyn _  _ _ _               ) = []
  dataDeclsOfTypeDecl (TypeNew tn _ _ (NewCons cn _ _)) =
    [(tn, [(cn,1)])] -- should not be relevant since newtypes are eliminated
  dataDeclsOfTypeDecl (Type    tn _ _ cdecl           ) =
    [(tn, map (\ (Cons cn ar _ _) -> (cn,ar)) cdecl)]

------------------------------------------------------------------------------
