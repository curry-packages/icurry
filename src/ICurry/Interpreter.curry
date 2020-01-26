------------------------------------------------------------------------------
--- A simple interpreter for ICurry based on the a small-step semantics.
---
-- The following invariants are required for ICurry programs:
-- 1. No nested case expression
-- 2. If there is a case expression, it is on some argument and the
--    argument index is contained in the demand information of the function.
------------------------------------------------------------------------------

module ICurry.Interpreter
 where

import List   ( init, isPrefixOf, last, replace )
import System ( sleep, system )

import ICurry.Types
import ICurry.Graph
import ICurry.Compiler

------------------------------------------------------------------------------
-- The options of the ICurry interpreter.
data IOptions = IOptions
  { verbosity   :: Int    -- verbosity: 0=quiet, 4=all
  , showAllExps :: Bool   -- show all expressions represented by the graph
  , withGraph   :: Bool   -- should we visualize the graph?
  , withViewer  :: String -- program to view PDFs (default: "evince")
  , fullGraph   :: Bool   -- should we show the full graph with all nodes?
  , interactive :: Bool   -- ask user to proceed after each step?
  , waitTime    :: Int    -- seconds to wait in non-interactive mode
  , makePDF     :: Bool   -- generates a PDF containing all graph steps
  , stepNum     :: Int    -- step number (internal)
  }

-- Default options: quiet non-interactive mode
defOpts :: IOptions
defOpts = IOptions 0 False False "evince" False False 0 False 0

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

-- Print the current state of the interpreter according to the given options.
printState :: IOptions -> State -> IO ()
printState opts st = do
  when (verb>3) $ putStr $ unlines
    [ "RAW GRAPH   : " ++ show (graph st)
    , "TASKS       : " ++ show tsks
    ]
  when (showAllExps opts) $ putStr $ unlines $
    "ALL EXPRESSIONS:" : map (showGraphExp (graph st)) (rootsOfState st)
  when (verb>0) $
    case tsks of
      [] -> putStrLn "NO TASK"
      tsk@(Task ctrl _ fp) : _ -> putStr $ unlines $
        [ "CURRENT TASK:"
        , "MAIN EXPR   : " ++ showGraphExp (graph st) (rootOfTask tsk) ] ++
        if (verb>1)
          then [ "CONTROL     : " ++ showControl ctrl
               , "FINGER PRINT: " ++ show fp ]
          else []
  when (withGraph opts || makePDF opts) $ showStateGraph
  when (waitTime opts > 0 && not (interactive opts)) $ sleep (waitTime opts)
  when (verb>0) $ putStrLn ""
 where
  verb = verbosity opts
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
            (graphToDot (graph st) ndcolors (verb > 3) (fullGraph opts))
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
  if interactive opts
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
execProg :: IOptions -> String -> String -> IO ()
execProg opts progname fname = do
  iprog <- icCompile defaultICOptions progname
  execIProg opts iprog fname

-- An interpreter for ICurry programs.
-- Executes a program with a main function where the name is provided
-- as a string.
-- It also prints intermediate steps, PDFs, etc. accordding to the options.
execIProg :: IOptions -> IProg -> String -> IO ()
execIProg opts (IProg _ _ _ ifuns) f = do
  let (g,ni) = addNode (FuncNode f []) emptyGraph
      opts1  = if makePDF opts then opts { stepNum = 1, withGraph = False }
                               else opts
  when (withGraph opts1) $
    viewDot (Just $ withViewer opts) 0 (graphToDot g [] (verbosity opts1 > 3)
            (fullGraph opts))
  let allfuns = ifuns ++ standardFuncs
  opts2 <- runWith opts1 (initState allfuns g ni)
  when (makePDF opts2) $ do
    -- Concatenate all step PDFs into on PDF:
    let pdffiles = map (\i -> "ICURRYDOT" ++ show i ++ ".pdf")
                       [1 .. stepNum opts2]
        pdfmain  = "ICURRYSTEPS.pdf"
    system $ unwords $ "pdftk" : pdffiles ++ ["cat","output",pdfmain]
    system $ unwords $ "/bin/rm -f" : pdffiles
    putStrLn $ "PDFs of all steps written to file '" ++ pdfmain ++ "'."

runWith :: IOptions -> State -> IO IOptions
runWith opts st
  | null (tasks st)
  = do printState opts st
       return opts
  | otherwise
  = do printState opts st
       procstep <- if verbosity opts > 0 then askProceed opts else return True
       if not procstep
         then return opts
         else do
           let num   = stepNum opts
               nopts = if num==0 then opts else opts { stepNum = num + 1 }
               nst   = step st
           maybe (runWith nopts nst)
                 (\nid -> do putStrLn $ "RESULT: " ++
                                        showGraphExp (graph nst) nid
                             proceed <- askProceed opts
                             if proceed
                               then runWith nopts nst {currResult = Nothing}
                               else return opts)
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
evalFirstTask _ [] = error "step: empty tasks"
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

evalFirstTask st (Task (IBlockEnv (IBlock vs asgns stm) ienv) stk fp : tsks) =
  let (g1,ienv1) = addAssigns (graph st) asgns (map toNull vs ++ ienv) in
  case stm of
    IExempt -> st { tasks = tsks }  -- failure: remove current task
  
    IReturn iexp -> -- return: replace ROOT node
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
 where
  toNull (IVarDecl  v) = (v, 0)
  toNull (IFreeDecl v) = (v, 0)

-- This operation is used when the control of the first task contains
-- a function node ready for execution, i.e., a possibly demanded argument
-- has been evaluated.
-- Then the control is replaced by the body of the function
-- (or by the result of executing some external operation).
invokeFunction :: State -> [Task] -> State
invokeFunction _ [] = error "invokeFunction: empty tasks"
invokeFunction st (Task (CNode nid) stk fp : tsks) =
  case lookupNode nid (graph st) of
    FuncNode f ns -> case bodyOf f (program st) of
      IFuncBody blck ->
        let ienv = [(0, nid)]
        in st { tasks = Task (IBlockEnv blck ienv) stk fp : tsks }
      IExternal en -> case en of
        "apply" -> 
          let node = addPartialArg (lookupNode (ns!!0) (graph st)) (ns!!1)
          in st { graph = updateNode (graph st) nid node }
        "$!" -> st { graph = updateNode (graph st) nid (FuncNode "apply" ns) }
        "normalForm" -> case lookupNode (ns!!0) (graph st) of
          ConsNode c cargs ->
            let argsenv = zip [1..] cargs
                evalcargs = foldl (\xs x -> IFCall ("","$!",0)
                                              [xs, IFCall ("","normalForm",0)
                                                          [IVar (fst x)]])
                                  (ICPCall ("",c,0) (length cargs) []) argsenv
                (g1,nexp) = extendGraph (graph st) argsenv evalcargs
            in st { graph = either (error "Internal error in normalForm")
                                   (updateNode g1 nid) nexp }
          _ -> error "step: use of 'normalForm' without constructor argument"
        _    -> error $ "step: unknown external function: " ++ en
    _ -> error "invokeFunction: no function node in control"
invokeFunction _ (Task (IBlockEnv _ _) _ _ : _) =
  error "invokeFunction: no function node in control"

-- Selects the constructor branch corresponding to some constructor node.
selectConsBranch :: Node -> [IConsBranch] -> IBlock
selectConsBranch nd [] =
  error $ "selectConsBranch: no branch for node: " ++ show nd
selectConsBranch nd (IConsBranch (_,c,_) blck : branches) = case nd of
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

-- Adds assignments to the environment and graph.
addAssigns :: Graph -> [IAssign] -> IEnv -> (Graph,IEnv)
addAssigns g [] env = (g,env)
addAssigns g (IVarAssign v e : asgns) env =
  let (g1,ne)  = extendGraph g env e
      (g2,nid) = either (\ni -> (g1,ni)) (\nd -> addNode nd g1) ne
  in addAssigns g2 asgns (updateEnv env v nid)
addAssigns _ (INodeAssign _ [] _ : _) _ =
  error "addAssigns: empty path"
addAssigns g (INodeAssign v path@(_:_) e : asgns) env =
  let n        = followPath g (lookupInEnv v env) (init path)
      (g1,ne)  = extendGraph g env e
      (g2,nid) = either (\ni -> (g1,ni)) (\nd -> addNode nd g1) ne
  in addAssigns (replaceNodeArg g2 n (last path) nid) asgns env

-- Replaces the i-th successor of node `nid` by node `narg`.
replaceNodeArg :: Graph -> NodeID -> Int -> NodeID -> Graph
replaceNodeArg g nid i narg = case lookupNode nid g of
  ConsNode c ns    -> updateNode g nid (ConsNode c (replace narg i ns))
  FuncNode f ns    -> updateNode g nid (FuncNode f (replace narg i ns))
  PartNode f m ns  -> updateNode g nid (PartNode f m (replace narg i ns))
  ChoiceNode _ _ _ -> error "replaceNodeArg: ChoiceNode"

-- Follows a path from a given node.
followPath :: Graph -> NodeID -> [Int] -> NodeID
followPath _ n [] = n
followPath g n (i:is) = case lookupNode n g of
  ConsNode _ ns      -> followPath g (selectArg ns) is
  FuncNode _ ns      -> followPath g (selectArg ns) is
  PartNode _ _ ns    -> followPath g (selectArg ns) is
  ChoiceNode _ n1 n2 -> followPath g (if i==0 then n1 else n2) is
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
extendGraph g0 env (IFCall (_,c,_) es) =
  let (g1,ns) = extendGraphL g0 env es
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
extendGraph g0@(Graph _ m) env (IOr e1 e2) =
  let (g1,[n1,n2]) = extendGraphL g0 env [e1,e2]
  in (g1, Right $ ChoiceNode m n1 n2) -- TODO: better choice ids

extendGraphL :: Graph -> IEnv -> [IExpr] -> (Graph,[NodeID])
extendGraphL g0 _ [] = (g0,[])
extendGraphL g0 env (e:es) =
  let (g1,n1) = extendGraph g0 env e
      (g2,n ) = either (\nid -> (g1,nid)) (\nd -> addNode nd g1) n1
      (g3,ns) = extendGraphL g2 env es
  in (g3, n:ns)

-- Shows a literal as a string.
showILit :: ILiteral -> String
showILit (IInt   n) = show n
showILit (IChar  c) = show c
showILit (IFloat f) = show f

------------------------------------------------------------------------------

-- Returns the function with a given (unqualified) name.
funcOf :: String -> [IFunction] -> IFunction
funcOf fn [] = error $ "Function '" ++ fn ++ "' not found!"
funcOf fn (fd@(IFunction (_,f,_) _ _ _ _) : funs) =
   if fn==f then fd else funcOf fn funs

bodyOf :: String -> [IFunction] -> IFuncBody
bodyOf fn prog = let IFunction _ _ _ _ b = funcOf fn prog in b

demandOf :: String -> [IFunction] -> Maybe Int
demandOf fn prog = case d of
  []  -> Nothing
  [i] -> Just i
  _   -> error $ "Function '" ++ fn ++
                 "' has more than one demanded argument (not yet supported)"
 where
  IFunction _ _ _ d _ = funcOf fn prog

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

-- normalForm x: demands x and returns the normal form of x
funNormalForm :: IFunction
funNormalForm =
  IFunction ("Prelude","normalForm",0) 1 Public [0] (IExternal "normalForm")

standardFuncs :: [IFunction]
standardFuncs = [funApply, funSeq, funDollarBang, funNormalForm]

------------------------------------------------------------------------------
