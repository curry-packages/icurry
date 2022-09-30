------------------------------------------------------------------------------
--- A simple interpreter for ICurry based on the a small-step semantics.
---
--- The following invariants are required for ICurry programs:
--- 1. No nested case expression
--- 2. If there is a case expression, it is on some argument and the
---    argument index is contained in the demand information of the function.
---
--- @author Michael Hanus, Sascha Ecks
--- @version July 2022
------------------------------------------------------------------------------

module ICurry.Interpreter
 where

import Control.Monad  ( when, unless )
import Data.List      ( init, isPrefixOf, last, replace )
import System.Process ( sleep, system )

import ICurry.Types
import ICurry.Graph
import ICurry.Compiler ( icCompile )
import ICurry.Options  ( ICOptions(..), defaultICOptions )
import qualified TermGraph.XML as TG

------------------------------------------------------------------------------
-- The options of the ICurry interpreter.
data IOptions = IOptions
  { icOptions   :: ICOptions -- inherit options of the ICurry compiler
  , showAllExps :: Bool      -- show all expressions represented by the graph
  , waitTime    :: Int       -- seconds to wait in non-interactive mode
  , stepNum     :: Int       -- step number (internal)
  }

-- Default options: quiet non-interactive mode
defOpts :: IOptions
defOpts = IOptions defaultICOptions False 0 0

withGraph :: IOptions -> Int
withGraph opts = optShowGraph (icOptions opts)

------------------------------------------------------------------------------

-- The finger print is a partial mapping from choice identifiers to integers.
type FingerPrint = [(ChoiceID,Int)]

data Control = CNode NodeID | IBlockEnv IBlock IEnv
 deriving Show

-- An environment is a mapping from IVars to node identifiers.
type IEnv = [(IVarIndex, NodeID)]

lookupInEnv :: IVarIndex -> IEnv -> NodeID
lookupInEnv v env =
  maybe (error "Variable not found in environment")
        id
        (lookup v env)

updateEnv :: IEnv -> IVarIndex -> NodeID -> IEnv
updateEnv []             v n = [(v,n)]
updateEnv ((v',m) : env) v n =
  if v==v' then (v,n) : env
           else (v',m) : updateEnv env v n

-- A task of the execution contains the control,
-- a stack of function nodes together with the index of the demanded argument,
-- and a finger print.
data Task = Task Control [(NodeID,Int)] FingerPrint
  deriving Show

-- Returns the root node of the expression to be evaluated by a task.
rootOfTask :: Task -> NodeID
rootOfTask (Task ctrl stk _)
  | null stk  = case ctrl of CNode nid -> nid
                             IBlockEnv _ env -> lookupInEnv 0 env
  | otherwise = fst (last stk)

-- Returns the node currently evaluated.
currentNodeOfTask :: Task -> NodeID
currentNodeOfTask (Task (CNode nid) _ _)       = nid
currentNodeOfTask (Task (IBlockEnv _ env) _ _) = lookupInEnv 0 env

------------------------------------------------------------------------------
-- The state of an ICurry program under evaluation as described in the
-- WFLP'19 paper.
-- The auxiliary component `currResult` is set in a step when a new result
-- has been computed.
data State = State { program :: [IFunction]
                   , graph   :: Graph
                   , tasks   :: [Task]
                   , results :: [NodeID]
                   , currResult :: Maybe NodeID
                   }
 deriving Show



-- Initial state for a program, graph, and root node id.
initState :: [IFunction] -> Graph -> NodeID -> State
initState prog graph nid = State prog graph [Task (CNode nid) [] []] [] Nothing

-- Returns the root nodes of all results and all expressions.
rootsOfState :: State -> [NodeID]
rootsOfState st = results st ++ map rootOfTask (tasks st)

-- Show all results stored in a state.
showResults :: State -> String
showResults st = unlines (map (showGraphExp (graph st)) (results st))

-- Adds a result to a program state.
addResult :: NodeID -> State -> State
addResult nid st = st { results = results st ++ [nid], currResult = Just nid }

getTGState :: ICOptions -> State -> [TG.State]
getTGState icopts st
  | optTermGraph icopts = case (tasks st) of
    (Task (CNode nid) _ fp) : _ -> [ TG.State
                                        (reachableGraph (graph st) [graphRoot (graph st)])
                                        nid
                                        (results st)
                                        fp ]
    []                          -> [ TG.State
                                        (reachableGraph (graph st) [graphRoot (graph st)])
                                        0
                                        (results st)
                                        [] ]
    _                           -> []
  | otherwise = []

-- Print the current state of the interpreter according to the given options.
printState :: IOptions -> State -> IO ()
printState opts st = do
  when (verb > 2) $ putStr $ unlines
    [ "RAW GRAPH   : " ++ show (graph st)
    , "TASKS       : " ++ show tsks
    ]
  when (showAllExps opts) $ putStr $ unlines $
    "ALL EXPRESSIONS:" : map (showGraphExp (graph st)) (rootsOfState st)
  when (verb == 1) $ case tsks of
    []    -> putStrLn "NO TASK"
    tsk:_ -> putStr $ unlines $
               [ "CURRENT EXPR: " ++ showGraphExp (graph st) (rootOfTask tsk) ]
  when (verb > 1) $
    case tsks of
      []                       -> putStrLn "NO TASK"
      tsk@(Task ctrl _ fp) : _ -> putStr $ unlines $
        [ "CURRENT TASK:"
        , "MAIN EXPR   : " ++ showGraphExp (graph st) (rootOfTask tsk) ] ++
        if verb > 2 then [ "CONTROL     : " ++ showControl ctrl
                         , "FINGER PRINT: " ++ show fp ]
                    else []
  when (withGraph opts > 0 ||
        not (null (optOutput (icOptions opts)))) $ showStateGraph
  when (waitTime opts > 0 && not (optInteractive (icOptions opts))) $
    sleep (waitTime opts)
  when (verb > 1) $ putStrLn ""
 where
  verb = optVerb (icOptions opts)
  tsks = tasks st

  showControl (CNode nid) = "NODE: " ++ show nid
  showControl (IBlockEnv b e) = "BLOCK: " ++ show b ++
                                "\n              ENV: " ++ show e

  -- Visualize the graph contained in the current state as a dot graph.
  showStateGraph = do
    let ndcolors =
          (if null tsks then id
                        else markCurrent (currentNodeOfTask (head tsks)))
             (map (\ (i,t) -> (rootOfTask t,
                               [("color",if i==1 then "red" else "blue")]))
                  (zip [1..] tsks)) ++
          map (\n -> (n,[("color","green"),("style","filled")])) (results st)
    viewDot Nothing (stepNum opts)
            (graphToDot (graph st) ndcolors (withGraph opts > 2)
                        (withGraph opts > 1))
   where
    markCurrent cn [] = [(cn, yellowFill)]
    markCurrent cn ((nid,nas) : ncs)
      | nid == cn = (nid, nas ++ yellowFill) : ncs
      | otherwise = (nid, nas): markCurrent cn ncs

    yellowFill = [("fillcolor","yellow"),("style","filled")]

{-
The following coloring is used in the graph:

- red node: root of the active task
- blue node: root of an inactive task
- green node: root of a computed result
- yellow filled node: root of the current contol

-}

askProceed :: IOptions -> IO Bool
askProceed opts =
  if optInteractive (icOptions opts)
    then do putStr "Proceed (<RET>) or abort (a)? "
            ans <- getLine
            if null ans
              then return True
              else if ans `isPrefixOf` "abort"
                     then putStrLn "Execution aborted!" >> return False
                     else askProceed opts
    else return True

------------------------------------------------------------------------------
-- An interpreter for a single Curry program based on translating
-- them into ICurry.
-- The program name and the unqualified name of the main function
-- must be provided as string arguments.
-- It also prints intermediate steps, PDFs, etc. accordding to the options.
execProg :: IOptions -> String -> String -> IO ([TG.State])
execProg opts progname fname = do
  iprog <- icCompile defaultICOptions progname
  execIProg opts iprog fname

-- An interpreter for ICurry programs.
-- Executes a program with a main function where the name is provided
-- as a string.
-- It also prints intermediate steps, PDFs, etc. accordding to the options.
execIProg :: IOptions -> IProg -> String -> IO ([TG.State])
execIProg opts (IProg _ _ _ ifuns) f = do
  let (g,ni)  = addNode (FuncNode f []) emptyGraph
      pdfmain = optOutput (icOptions opts)
      opts1   = if null pdfmain
                  then opts
                  else opts { icOptions = (icOptions opts) { optShowGraph = 0 }
                            , stepNum = 1 }
  when (withGraph opts1 > 0) $
    viewDot (Just $ optViewPDF (icOptions opts)) 0 (graphToDot g []
            (withGraph opts1 > 2)
            (withGraph opts1 > 1))
  let allfuns = ifuns ++ standardFuncs
  (opts2,states) <- runWith opts1 (initState allfuns g ni) []
  unless (null pdfmain) $ do
    -- Concatenate all step PDFs into on PDF:
    let pdffiles = map (\i -> "ICURRYDOT" ++ show i ++ ".pdf")
                       [1 .. stepNum opts2]
    system $ unwords $ "pdftk" : pdffiles ++ ["cat", "output", pdfmain]
    system $ unwords $ "/bin/rm -f" : pdffiles
    putStrLn $ "PDFs of all steps written to '" ++ pdfmain ++ "'."
  return states

runWith :: IOptions -> State -> [TG.State] -> IO (IOptions, [TG.State])
runWith opts st states
  | optMaxSteps (icOptions opts) == length states
  = do printState opts st
       return (opts, states)
  | null (tasks st)
  = do printState opts st
       let nstates = states ++ (getTGState (icOptions opts) st)
       return (opts, nstates)
  | otherwise
  = do printState opts st
       let nstates = states ++ (getTGState (icOptions opts) st)
       procstep <- if optVerb (icOptions opts) > 0 then askProceed opts
                                                   else return True
       if not procstep
         then return (opts, nstates)
         else do
           let num   = stepNum opts
               nopts = if num==0 then opts else opts { stepNum = num + 1 }
               nst   = step st
           maybe (runWith nopts nst nstates)
                 (\nid -> do putStrLn $ "RESULT: " ++
                                        showGraphExp (graph nst) nid
                             proceed <- askProceed opts
                             if proceed
                               then runWith nopts
                                            (nst {currResult = Nothing})
                                            nstates
                               else return (opts, nstates))
                 (currResult nst)

-- Evaluates a 0-ary function w.r.t. an ICurry program and returns
-- the list of all results formatted as strings.
-- Used for testing.
evalFun :: IProg -> String -> [String]
evalFun (IProg _ _ _ ifuns) f =
  let (g,ni) = addNode (FuncNode f []) emptyGraph
  in evaluate (initState ifuns g ni)
 where
  evaluate st
    | null (tasks st) = []
    | otherwise
    = let st' = step st
      in maybe (evaluate st')
               (\nid -> showGraphExp (graph st') nid :
                        evaluate st' {currResult = Nothing})
               (currResult st')

------------------------------------------------------------------------------
-- Implementation of the small-step semantics.

-- The small step.
step :: State -> State
step st = evalFirstTask st (tasks st)

-- The small step on the first task.
evalFirstTask :: State -> [Task] -> State
evalFirstTask _  [] = error "step: empty tasks"
evalFirstTask st (Task (CNode nid) stk fp : tsks) =
  case lookupNode nid (graph st) of
    ConsNode _ _ -> case stk of
      [] -> addResult nid (st { tasks = tsks })
      ((fnid,_) : rstk) ->
         let st1 = st { tasks = Task (CNode fnid) rstk fp : tsks }
         in invokeFunction st1 (tasks st1)

    -- partial calls are treated as constructors:
    PartNode _ _ _ -> case stk of
      [] -> addResult nid (st { tasks = tsks })
      ((fnid,_) : rstk) ->
         let st1 = st { tasks = Task (CNode fnid) rstk fp : tsks }
         in invokeFunction st1 (tasks st1)

    FuncNode f _ -> case demandOf f (program st) of
      Nothing -> invokeFunction st (tasks st)
      Just di -> let ni = followPath (graph st) nid [di]
                 in st { tasks = Task (CNode ni) ((nid,di) : stk) fp : tsks }

    ChoiceNode cid n1 n2 -> case stk of
        [] -> case lookup cid fp of
          Just c  -> let ns = if c==1 then n1 else n2
                     in st { tasks = Task (CNode ns) stk fp : tsks }
          Nothing -> let newtasks = [Task (CNode n1) [] ((cid,1) : fp),
                                     Task (CNode n2) [] ((cid,2) : fp)]
                     in st { tasks = tsks ++ newtasks }
        ((fnid,di) : nids) -> -- pull-tab step:
          let g0 = graph st in
          case lookupNode fnid g0 of
            FuncNode f ns ->
              let (g1,n1') = addNode (FuncNode f (replace n1 di ns)) g0
                  (g2,n2') = addNode (FuncNode f (replace n2 di ns)) g1
              in st { graph = updateNode g2 fnid (ChoiceNode cid n1' n2')
                    , tasks = Task (CNode fnid) nids fp : tsks }
            _ -> error "step: stack does not refer to function node"

    FreeNode -> case stk of
      [] -> addResult nid (st { tasks = tsks })
      ((fnid,_) : rstk) ->
         -- bind free node to choice structure corresponding to case expression
         maybe
           (let newtsks = Task (CNode fnid) rstk fp : tsks
            in invokeFunction (st { tasks = newtsks }) newtsks)
           (\chexp ->
             let (gr1,nd) = extendGraph (graph st) [] chexp
                 chnd = either (error "evalFirstTask: no choice") id nd
             in st { graph = updateNode gr1 nid chnd })
           (choiceOfDemand st fnid)

evalFirstTask st (Task (IBlockEnv (IBlock vs asgns stm) ienv) stk fp : tsks) =
  let (g0,ienv0) = addVarDecls (graph st) ienv vs
      (g1,ienv1) = addAssigns g0 ienv0 asgns in
  case stm of
    IExempt -> st { tasks = tsks }  -- failure: remove current task

    IReturn iexp -> -- return statement: replace current ROOT node
      let (g2,nexp)  = extendGraph g1 ienv1 iexp
          rootid     = lookupInEnv 0 ienv
      in either (\ni -> st { graph = replaceNode g2 rootid ni,
                                         tasks = Task (CNode ni) stk fp : tsks })
                (\nd -> st { graph = updateNode g2 rootid nd,
                             tasks = Task (CNode rootid) stk fp : tsks })
                nexp

    ICaseCons cv branches -> -- constructor case: select branch
      let bn    = lookupInEnv cv ienv1
          sb    = selectConsBranch (lookupNode bn g1) branches
      in st { graph = g1
            , tasks = Task (IBlockEnv sb ienv1) stk fp : tsks }

    ICaseLit cv branches -> -- literal case: select branch
      let bn = lookupInEnv cv ienv1
          sb = selectLitBranch (lookupNode bn g1) branches
      in st { graph = g1
            , tasks = Task (IBlockEnv sb ienv1) stk fp : tsks }

-- This operation is used when the control of the first task contains
-- a function node ready for execution, i.e., a possibly demanded argument
-- has been evaluated.
-- Then the control is replaced by the body of the function
-- (or by the result of executing some external operation).
invokeFunction :: State -> [Task] -> State
invokeFunction _ [] = error "invokeFunction: empty tasks"
invokeFunction st (Task (CNode nid) stk fp : tsks) =
  case lookupNode nid gr of
    FuncNode f ns -> case bodyOf f (program st) of
      IFuncBody blck ->
        let ienv = [(0, nid)]
        in st { tasks = Task (IBlockEnv blck ienv) stk fp : tsks }
      IExternal en -> case en of
        "normalForm" -> let nfarg = ns !! 0 in case lookupNode nfarg gr of
          ConsNode c cargs ->
            let argsenv = zip [1..] cargs
                evalcargs = foldl (\xs x -> IFCall ("","$$!",0)
                                              [xs, IFCall ("","normalForm",0)
                                                          [IVar (fst x)]])
                                  (ICPCall ("",c,0) (length cargs) []) argsenv
                (gr1,nexp) = extendGraph gr argsenv evalcargs
            in st { graph = either (error "Internal error in normalForm")
                                   (updateNode gr1 nid) nexp }
          FreeNode -> -- Warning: this does not work of free variable will be
                      -- later instantiated!
                      st { graph = replaceNode gr nid nfarg
                         , tasks = Task (CNode nfarg) stk fp : tsks }
          _ -> error "step: use of 'normalForm' without constructor argument"
        _ -> st { graph = updateNode gr nid (evalExternal gr en ns) }
    _ -> error "invokeFunction: no function node in control"
 where gr = graph st
invokeFunction _ (Task (IBlockEnv _ _) _ _ : _) =
  error "invokeFunction: no function node in control"

-- Evaluates an external function to a node containing the evaluated value.
-- The arguments are the current graph, the external name,
-- and the argument nodes.
evalExternal :: Graph -> String -> [NodeID] -> Node
evalExternal gr ename ns = case unQName ename of
  "apply" -> addPartialArg (lookupNode (ns!!0) gr) (ns!!1)
  "$!"    -> FuncNode "apply" ns
  "$#"    -> FuncNode "apply" ns
  "prim_Int_plus" ->
     ConsNode (show (lookupIntNode (ns!!0) gr + lookupIntNode (ns!!1) gr)) []
  "prim_Int_mult" ->
     ConsNode (show (lookupIntNode (ns!!0) gr * lookupIntNode (ns!!1) gr)) []
  _    -> error $ "step: unknown external function: " ++ ename
 where
  unQName s = let (mn,ufn) = break (=='.') s
              in if null ufn then mn else unQName (tail ufn)

lookupIntNode :: NodeID -> Graph -> Int
lookupIntNode nid gr = case lookupNode nid gr of
  ConsNode c [] -> read c :: Int
  _             -> error "lookupIntNode: no integer found"


-- Selects the constructor branch corresponding to some constructor node.
selectConsBranch :: Node -> [IConsBranch] -> IBlock
selectConsBranch nd [] =
  error $ "selectConsBranch: no branch for node: " ++ show nd
selectConsBranch nd (IConsBranch (_,c,_) _ blck : branches) = case nd of
  ConsNode nc _ -> if nc == c then blck
                              else selectConsBranch nd branches
  _             -> error $ "selectConsBranch: unevaluated branch node: " ++
                           show nd

-- Selects the literal branch corresponding to some literal node.
selectLitBranch :: Node -> [ILitBranch] -> IBlock
selectLitBranch nd [] =
  error $ "selectLitBranch: no branch for node: " ++ show nd
selectLitBranch nd (ILitBranch l blck : branches) = case nd of
  ConsNode nc _ -> if nc == showILit l then blck
                                       else selectLitBranch nd branches
  _             -> error $ "selectLitBranch: unevaluated branch node: " ++
                           show nd

-- Adds variable declarations to the graph and environment.
addVarDecls :: Graph -> IEnv -> [IVarDecl] -> (Graph,IEnv)
addVarDecls g env []                     = (g,env)
addVarDecls g env (IVarDecl  v : vdecls) = addVarDecls g ((v,0) : env) vdecls
addVarDecls g env (IFreeDecl v : vdecls) =
  let (g1,fn) = addNode FreeNode g
  in addVarDecls g1 ((v,fn) : env) vdecls

-- Adds assignments to the graph and environment.
addAssigns :: Graph -> IEnv -> [IAssign] -> (Graph,IEnv)
addAssigns g env [] = (g,env)
addAssigns g env (IVarAssign v e : asgns) =
  let (g1,ne)  = extendGraph g env e
      (g2,nid) = either (\ni -> (g1,ni)) (\nd -> addNode nd g1) ne
  in addAssigns g2 (updateEnv env v nid) asgns
addAssigns _ _ (INodeAssign _ [] _ : _) =
  error "addAssigns: empty path"
addAssigns g env (INodeAssign v path@(_:_) e : asgns) =
  let n        = followPath g (lookupInEnv v env) (init path)
      (g1,ne)  = extendGraph g env e
      (g2,nid) = either (\ni -> (g1,ni)) (\nd -> addNode nd g1) ne
  in addAssigns (replaceNodeArg g2 n (last path) nid) env asgns

-- Replaces the i-th successor of node `nid` by node `narg`.
replaceNodeArg :: Graph -> NodeID -> Int -> NodeID -> Graph
replaceNodeArg g nid i narg = case lookupNode nid g of
  ConsNode c ns    -> updateNode g nid (ConsNode c (replace narg i ns))
  FuncNode f ns    -> updateNode g nid (FuncNode f (replace narg i ns))
  PartNode f m ns  -> updateNode g nid (PartNode f m (replace narg i ns))
  ChoiceNode _ _ _ -> error "replaceNodeArg: ChoiceNode"
  FreeNode         -> error "replaceNodeArg: FreeNode"

-- Follows a path from a given node.
followPath :: Graph -> NodeID -> [Int] -> NodeID
followPath _ n [] = n
followPath g n (i:is) = case lookupNode n g of
  ConsNode _ ns      -> followPath g (selectArg ns) is
  FuncNode _ ns      -> followPath g (selectArg ns) is
  PartNode _ _ ns    -> followPath g (selectArg ns) is
  ChoiceNode _ n1 n2 -> followPath g (selectArg [n1,n2]) is
  FreeNode           -> error "followPath: FreeNode"
 where
  selectArg ns | i >= length ns = error "followPath: argument does not exist!"
               | otherwise      = ns !! i

-- Extends a graph w.r.t. a given environment and ICurry expression
-- so that a expression is represented in the graph.
-- The result is either a node identifier of an existing node (if the
-- expression already exists in graph)  or the contents of a new node
-- to be added.
-- Used for assignments and return statements (ISimpleBlock).
extendGraph :: Graph -> IEnv -> IExpr -> (Graph, Either NodeID Node)
extendGraph g0 env (IVar v) = (g0, Left $ lookupInEnv v env)
extendGraph g0 env (IVarAccess v path) =
  (g0, Left $ followPath g0 (lookupInEnv v env) path)
extendGraph g0 _ (ILit l) = (g0, Right $ ConsNode (showILit l) [])
extendGraph g0 env (IFCall (mn,c,_) es)
 | mn == "Prelude" && c == "unknown" && null es
 = (g0, Right FreeNode)
 | otherwise
 = let (g1,ns) = extendGraphL g0 env es
   in (g1, Right $ FuncNode c ns)
extendGraph g0 env (ICCall (_,c,_) es) =
  let (g1,ns) = extendGraphL g0 env es
  in (g1, Right $ ConsNode c ns)
extendGraph g0 env (IFPCall (_,c,_) m es) =
  let (g1,ns) = extendGraphL g0 env es
  in (g1, Right $ PartNode c (PartFuncCall m) ns)
extendGraph g0 env (ICPCall (_,c,_) m es) =
  let (g1,ns) = extendGraphL g0 env es
  in (g1, Right $ PartNode c (PartConsCall m) ns)
extendGraph g0 env (IOr e1 e2) =
  let (g1,[n1,n2]) = extendGraphL g0 env [e1,e2]
  in (g1, Right $ ChoiceNode (maxNodeID g1) n1 n2) -- TODO: better choice ids

extendGraphL :: Graph -> IEnv -> [IExpr] -> (Graph,[NodeID])
extendGraphL g0 _ [] = (g0,[])
extendGraphL g0 env (e:es) =
  let (g1,n1) = extendGraph g0 env e
      (g2,n ) = either (\nid -> (g1,nid)) (\nd -> addNode nd g1) n1
      (g3,ns) = extendGraphL g2 env es
  in (g3, n:ns)

-- Shows a literal as a string. Used in the interpreter to avoid
-- specific graph nodes for literal values.
showILit :: ILiteral -> String
showILit (IInt   n) = show n
showILit (IChar  c) = show c
showILit (IFloat f) = show f

------------------------------------------------------------------------------
-- The following operations retrieves some static information of programs.
-- In principle, they can be evaluated at compile time.
-- Since efficiency is not the objective of this interpreter,
-- we compute everything at run time.

-- Returns the function with a given (unqualified) name.
funcOf :: String -> [IFunction] -> IFunction
funcOf fn [] = error $ "Function '" ++ fn ++ "' not found!"
funcOf fn (fd@(IFunction (_,f,_) _ _ _ _) : funs) =
   if fn==f then fd else funcOf fn funs

-- Returns the body of a given function name.
bodyOf :: String -> [IFunction] -> IFuncBody
bodyOf fn prog = let IFunction _ _ _ _ b = funcOf fn prog in b

-- Returns the demanded argument of a given function name.
demandOf :: String -> [IFunction] -> Maybe Int
demandOf fn prog = case d of
  []  -> Nothing
  [i] -> Just i
  _   -> error $ "Function '" ++ fn ++
                 "' has more than one demanded argument (not yet supported)"
 where
  IFunction _ _ _ d _ = funcOf fn prog

-- Computes an expression representing the choice structure demanded
-- by the function of the given node id.
choiceOfDemand :: State -> NodeID -> Maybe IExpr
choiceOfDemand st nid =
  case lookupNode nid (graph st) of
    FuncNode f _ -> choiceOfBody (bodyOf f (program st))
    _            -> error "choiceOfDemand: no function node in control"
 where
  choiceOfBody (IFuncBody (IBlock _ _ stm)) = choiceOfStmt stm
  choiceOfBody (IExternal _)                = Nothing

  choiceOfStmt stm = case stm of
    ICaseCons _ bs ->
      if null bs
        then Nothing
        else Just (foldr1 (\e1 e2 -> IOr e1 e2) (map branchesToConsFree bs))
    _ -> error "choiceOfDemand: function without constructor demand in control"
   where
    branchesToConsFree (IConsBranch c ar _) =
      ICCall c (map (\_ -> IFCall ("Prelude","unknown",0) []) [1 .. ar])

------------------------------------------------------------------------------
-- Some standard functions which are usually defined in the prelude.
-- For the moment, when we compile single modules only, we define
-- them here since they are required for interpreter examples.

-- apply f x: demands f and returns  (f x)
funApply :: IFunction
funApply = IFunction ("Prelude","apply",0) 2 Public [0] (IExternal "apply")

-- seq x y: demands x and returns y
funSeq :: IFunction
funSeq = IFunction ("Prelude","seq",0) 2 Public [0] $ IFuncBody $
  IBlock [] [] (IReturn (IVarAccess 0 [1]))

-- f $! x: demands x and returns (f x)
funDollarBang :: IFunction
funDollarBang = IFunction ("Prelude","$!",0) 2 Public [1] (IExternal "$!")

-- f $$! x = f (id $! x), i.e., first f and then x is demanded, returns (f x).
-- Used for computations of normal forms with left to right argument evaluation.
funDollarDollarBang :: IFunction
funDollarDollarBang = IFunction ("Prelude","$$!",0) 2 Public [0] $ IFuncBody $
  IBlock [IVarDecl 1,IVarDecl 2]
         [IVarAssign 1 (IVarAccess 0 [0]),IVarAssign 2 (IVarAccess 0 [1])]
         (IReturn (IFCall ("Prelude","$!",0) [IVar 1, IVar 2]))

-- f $# x: demands x and returns (f x) (and suspends on a free variable
-- which is not yet implemented)
funDollarHash :: IFunction
funDollarHash = IFunction ("Prelude","$#",0) 2 Public [1] (IExternal "$#")

-- normalForm x: demands x and returns the normal form of x
funNormalForm :: IFunction
funNormalForm =
  IFunction ("Prelude","normalForm",0) 1 Public [0] (IExternal "normalForm")

standardFuncs :: [IFunction]
standardFuncs =
  [ funApply, funSeq, funDollarBang, funDollarDollarBang
  , funDollarHash, funNormalForm ]

------------------------------------------------------------------------------
