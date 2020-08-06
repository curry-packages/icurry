------------------------------------------------------------------------------
--- This module contains a simple compiler from FlatCurry to ICurry programs.
---
--- To do in future work:
--- * compile also imported modules depending on their date
--- * remove declarations/assignments of unused variables in ICurry code
---
--- @author Michael Hanus
--- @version August 2020
------------------------------------------------------------------------------

module ICurry.Compiler
 ( icCompile, flatCurry2ICurry, ICOptions(..), defaultICOptions
 , printStatus, printIntermediate )
 where

import List              ( elemIndex, maximum )

import FlatCurry.Files   ( readFlatCurry )
import FlatCurry.Goodies ( allVars, consName, funcName, funcVisibility
                         , progFuncs, progImports, progTypes )
import FlatCurry.Pretty  ( defaultOptions, ppProg )
import FlatCurry.Types
import Text.Pretty       ( pPrint )

import FlatCurry.CaseCompletion
import FlatCurry.CaseLifting ( defaultLiftOpts, defaultNoLiftOpts, liftProg )

import ICurry.Files  ( iCurryFileName, writeICurryFile )
import ICurry.Pretty ( ppIProg )
import ICurry.Types

test :: String -> IO ()
test p = do
  iprog <- icCompile defaultICOptions { optVerb = 3 } p
  writeICurryFile (iCurryFileName p) iprog
  putStrLn $ "ICurry program written to '" ++ iCurryFileName p ++ "'"

------------------------------------------------------------------------------
--- Generates an ICurry program by reading a FlatCurry program and
--- compiling it to ICurry.
icCompile :: ICOptions -> String -> IO IProg
icCompile opts p = do
  printStatus opts $ "Reading FlatCurry program '" ++ p ++ "'..."
  prog <- readFlatCurry p
  flatCurry2ICurry opts prog

--- Translates a FlatCurry program into an ICurry program.
--- It also reads the imported modules in order to access their
--- data and function declarations.
flatCurry2ICurry :: ICOptions -> Prog -> IO IProg
flatCurry2ICurry opts prog = do
  let impmods = progImports prog
  printStatus opts $ "Reading imported FlatCurry modules: " ++ unwords impmods
  impprogs <- mapM readFlatCurry impmods
  let datadecls = concatMap dataDeclsOf (prog : impprogs)
      ccprog    = completeProg (CaseOptions datadecls) prog
      clprog    = if optLift opts
                    then liftProg defaultLiftOpts ccprog
                    else liftProg defaultNoLiftOpts ccprog
  printDetails opts $ 
    textWithLines "Transformed FlatCurry program to be compiled:" ++
    pPrint (ppProg FlatCurry.Pretty.defaultOptions clprog)
  let consmap   = concatMap consMapOfProg (prog : impprogs)
      impfunmap = concatMap publicFunMapOfProg impprogs
      pubfunmap = publicFunMapOfProg prog
      funmap    = pubfunmap ++ privateFunMapOfProg clprog pubfunmap ++ impfunmap
  let icprog = flat2icurry (opts { optConsMap = consmap, optFunMap = funmap})
                           clprog
  printIntermediate opts $
    textWithLines "Generated ICurry program:" ++
    pPrint (ppIProg icprog)
  printDetails opts (textWithLines "Generated ICurry file:" ++ showIProg icprog)
  return icprog
 where
  consMapOfProg fcy =
    concatMap (\ (_,cars) -> map (\ ((cname,car),pos) -> (cname,(car,pos)))
                                 (zip cars [0..]))
              (dataDeclsOf fcy)

  -- compute mapping of public function names to indices
  publicFunMapOfProg fcprog =
    zip (map funcName
             (filter (\f -> funcVisibility f == FlatCurry.Types.Public)
                     (progFuncs fcprog)))
        [0..]

  privateFunMapOfProg fcprog pubfunmap =
    zip (filter (\fn -> fn `notElem` map fst pubfunmap)
                (map funcName (progFuncs fcprog)))
        [(length pubfunmap) ..]

  textWithLines s = unlines [l, s, l]
   where l = take 78 (repeat '-')


------------------------------------------------------------------------------
--- Options for the ICurry compiler.
--- Contains mappings from constructor and functions names
--- into locally unique integers and other stuff.
data ICOptions = ICOptions
  { optVerb        :: Int    -- verbosity
                             -- (0: quiet, 1: status, 2: intermediate, 3: all)
  , optHelp        :: Bool   -- if help info should be printed
  , optLift        :: Bool   -- should nested cases/lets be lifted to top-level?
  , optMain        :: String -- name of main function
  , optShowGraph   :: Bool   -- visualize graph during execution?
  , optViewPDF     :: String -- command to view graph PDF
  , optInteractive :: Bool   -- interactive execution?
  , optVarDecls    :: Bool   -- optimize variable declarations?
  -- internal options
  , optConsMap   :: [(QName,(IArity,Int))] -- map: cons. names to arity/position
  , optFunMap    :: [(QName,Int)]       -- map: function names to module indices
  , optFun       :: QName  -- currently compiled function
  }

defaultICOptions :: ICOptions
defaultICOptions =
  ICOptions 1 False True "" False "evince" False False [] [] ("","")

-- Lookup arity and position index of a constructor.
arityPosOfCons :: ICOptions -> QName -> (IArity,Int)
arityPosOfCons opts qn =
  maybe (error $ "Internal error in ICurry.Compiler: arity of " ++
                 showQName qn ++ " is unknown")
        id
        (lookup qn (optConsMap opts))

-- Lookup position index of a constructor.
posOfCons :: ICOptions -> QName -> Int
posOfCons opts qn = snd (arityPosOfCons opts qn)

posOfFun :: ICOptions -> QName -> Int
posOfFun opts qn =
  maybe (error $ "Internal error in ICurry.Compiler: arity of " ++
                 showQName qn ++ " is unknown")
        id
        (lookup qn (optFunMap opts))

printStatus :: ICOptions -> String -> IO ()
printStatus opts s = when (optVerb opts > 0) $ putStrLn s

printIntermediate :: ICOptions -> String -> IO ()
printIntermediate opts s = when (optVerb opts > 1) $ putStrLn s

printDetails :: ICOptions -> String -> IO ()
printDetails opts s = when (optVerb opts > 2) $ putStrLn s

funError :: ICOptions -> String -> _
funError opts err = error $ "Function '" ++ snd (optFun opts) ++ "': " ++ err

------------------------------------------------------------------------------
--- Translation from FlatCurry to ICurry according to the transformation
--- specified in the ICurry paper.
flat2icurry :: ICOptions -> Prog -> IProg
flat2icurry opts (Prog modname imps types funs _) =
  IProg modname imps
        (concatMap trTypeDecl (zip [0..] types))
        (map (trFunc opts) funs)
 where
  trTypeDecl (_,  TypeSyn _ _ _ _)   = []
  trTypeDecl (ti, Type (mn,tn) _ _ cdecl) =
    [IDataType (mn,tn,ti)
               (map (\ (i, Cons (cmn,cn) ar _ _) -> ((cmn,cn,i),ar))
                    (zip [0..] cdecl))]

trVis :: Visibility -> IVisibility
trVis FlatCurry.Types.Public  = ICurry.Types.Public
trVis FlatCurry.Types.Private = ICurry.Types.Private

trFunc :: ICOptions -> FuncDecl -> IFunction
trFunc opts (Func qn@(mn,fn) ar vis _ rule) =
  IFunction (mn, fn, posOfFun opts qn) ar (trVis vis) (demandOf rule)
            (trRule optsf rule)
 where
  optsf = opts { optFun = qn }

-- Computes (approximates) the arguments demanded by a rule.
demandOf :: Rule -> [Int]
demandOf (External _) = [] -- TODO
demandOf (Rule args rhs) = case rhs of
  Case _ (Var v) _ -> maybe [] (: []) (elemIndex v args)
  _                -> []

trRule :: ICOptions -> Rule -> IFuncBody
trRule _ (External s) = IExternal s
trRule opts (Rule args rhs) = IFuncBody (toIBlock opts args rhs 0)

toIBlock :: ICOptions -> [VarIndex] -> Expr -> Int -> IBlock
toIBlock opts vs e root =
  IBlock (if optVarDecls opts
            then varDecls
            else map IVarDecl (filter (`elem` evars) vs) ++ varDecls)
         (map (\ (p,i) -> IVarAssign i (IVarAccess root [p]))
              (filter ((`elem` evars) . snd) (zip [0..] vs)) ++
          fst varAssigns ++ map fst (snd varAssigns))
         (case e of
            Case _ ce brs@(Branch (Pattern _ _) _ : _) ->
              let carg = trCaseArg ce
              in ICaseCons carg (map (trPBranch carg) brs)
            Case _ ce brs@(Branch (LPattern _ ) _ : _) ->
              let carg = trCaseArg ce
              in ICaseLit carg (map (trLBranch carg) brs)
            Comb FuncCall fn [] | fn == pre "failed" -> IExempt
            _ -> IReturn (toIExpr opts e))
 where
  evars = allVars e

  varDecls = case e of
    Free fvs _       -> map IFreeDecl fvs
    Let bs   _       -> if optVarDecls opts
                          then map IVarDecl
                                   (filter (`elem` cyclicVars) (map fst bs))
                          else map (IVarDecl . fst) bs
    Case _ (Var _) _ -> []
    Case _ _       _ -> if optVarDecls opts then [] else [IVarDecl caseVar]
    _                -> []

  -- fresh variable to translate complex case arguments:
  caseVar = maximum (0 : evars) + 1

  -- the assignments for this block: a pair of direct assignments
  -- and subsequent assignments required for recursive lets
  -- (where the cyclic variables is returned)
  varAssigns = case e of
    Let bs _ ->
      let assigns = map (\ (v,b) -> (v, toIExpr opts b)) bs
      in (map (\ (v,be) -> IVarAssign v be) assigns,
          -- add assignments of recursive occurrences:
          recursiveAssigns assigns)
    Case _ (Var _) _ -> ([], [])
    Case _ ce      _ -> ([IVarAssign caseVar (toIExpr opts ce)], [])
    _                -> ([], [])
   where
    recursiveAssigns [] = []
    recursiveAssigns (ve:ves) =
      let vps = varPos [] (snd ve)
      in map (\ (v,p) -> (INodeAssign (fst ve) p (IVar v), v))
             (filter (\vp -> fst vp `elem` map fst (ve:ves)) vps) ++
         recursiveAssigns ves

  -- variables used to implement cyclic data structures
  cyclicVars = map snd (snd varAssigns)

  trCaseArg ce = case ce of
                   Var v -> v
                   _     -> caseVar

  trPBranch carg (Branch (Pattern qn@(mn,cn) pvs) be) =
    let (ar,pos) = arityPosOfCons opts qn
    in IConsBranch (mn, cn, pos) ar (toIBlock opts pvs be carg)
  trPBranch _ (Branch (LPattern _) _) = funError opts "trPBranch with LPattern"

  trLBranch carg (Branch (LPattern lit) be) =
    ILitBranch (trLit lit) (toIBlock opts [] be carg)
  trLBranch _ (Branch (Pattern _ _) _) = funError opts "trLBranch with Pattern"

toIExpr :: ICOptions -> Expr -> IExpr
toIExpr _ (Var v) = IVar v
toIExpr _ (Lit l) = ILit (trLit l)
toIExpr opts (Comb ct qn@(mn,fn) es)
 | qn == pre "?" && length es == 2
 = toIExpr opts (Or (es!!0) (es!!1))
 | otherwise
 = let icall = case ct of
                 FuncCall       -> IFCall  (mn, fn, posOfFun opts qn)
                 ConsCall       -> ICCall  (mn, fn, posOfCons opts qn)
                 FuncPartCall m -> IFPCall (mn, fn, posOfFun opts qn) m
                 ConsPartCall m -> ICPCall (mn, fn, posOfCons opts qn) m
   in icall (map (toIExpr opts) es)
toIExpr opts (Or e1 e2)   = IOr (toIExpr opts e1) (toIExpr opts e2)
toIExpr opts (Typed e _)  = toIExpr opts e
toIExpr opts (Let _ e)    = toIExpr opts e
toIExpr opts (Free _ e)   = toIExpr opts e
toIExpr opts (Case _ _ _) = funError opts "toIExpr: Case occurred"

trLit :: Literal -> ILiteral
trLit (Intc i)   = IInt i
trLit (Floatc f) = IFloat f
trLit (Charc c)  = IChar c

-- Extracts the variables and their positions occurring in an ICurry expression
varPos :: [Int] -> IExpr -> [(IVarIndex,[Int])]
varPos rpos (IVar v) = [(v,rpos)]
varPos _    (IVarAccess _ _) = []
varPos _    (ILit _) = []
varPos rpos (IFCall _ args) = concatMap (\ (i,e) -> varPos (rpos ++ [i]) e)
                                        (zip [0..] args)
varPos rpos (ICCall qn args) = varPos rpos (IFCall qn args)
varPos rpos (IFPCall qn _ args) = varPos rpos (IFCall qn args)
varPos rpos (ICPCall qn _ args) = varPos rpos (IFCall qn args)
varPos rpos (IOr e1 e2) = varPos (rpos ++ [0]) e1 ++ varPos (rpos ++ [1]) e2

------------------------------------------------------------------------------
--- Simple show for ICurry programs.
showIProg :: IProg -> String
showIProg (IProg mn imps types funs) = unlines $
  unwords ["IProg", mn, show imps, show types] :
  "[" : map show funs ++ ["]"]

------------------------------------------------------------------------------
-- Auxiliaries:

showQName :: QName -> String
showQName (mn,fn) = mn ++ "." ++ fn

pre :: String -> QName
pre s = ("Prelude", s)

------------------------------------------------------------------------------
