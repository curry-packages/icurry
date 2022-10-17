------------------------------------------------------------------------------
--- An implementation of term graphs used by the ICurry interpreter.
---
--- @author Michael Hanus, Sascha Ecks
--- @version January 2022
------------------------------------------------------------------------------

module ICurry.Graph
 where

import Data.List      ( intercalate, nub )
import System.IO      ( hPutStr, hClose )

import Data.GraphViz as Dot
import System.IOExts  ( connectToCommand )
import System.Process ( system )

------------------------------------------------------------------------------
--- Views a dot graph as PDF.
--- If the first argument is `(Just c)`, the command `c`
--- (e.g., "evince" or "okular --noraise") to view the
--- generated PDF is started in background, otherwise the viewer is not invoked.
--- The second argument is the step number and used to index the output files
--- if it is positive.
viewDot :: Maybe String -> Int -> DotGraph -> IO ()
viewDot withviewer num =
  view . showDotGraphWithAttrs "ordering=out;\nfontsize=10;"
 where
  view dottxt = do
    let outpdf = "ICURRYDOT" ++ (if num>0 then show num else "") ++ ".pdf"
    dothdl <- connectToCommand $ "dot -Tpdf -o" ++ outpdf
    hPutStr dothdl dottxt
    hClose dothdl
    maybe (return ())
          (\c -> do
             system $ unwords [c, outpdf, "2> /dev/null", "> /dev/null", "&"]
             return ())
          withviewer

------------------------------------------------------------------------------
-- A simple implementation of graphs representing ICurry expressions
-- with sharing.
-- This implementation can be made more efficient by using finite maps
-- and "pointers" from target to source nodes (for the replacement operation).

-- Graph nodes are identified by integers.
type NodeID = Int

-- Choice identifiers are implemented as integers.
type ChoiceID = Int

-- Representation of partial function or constructor calls
-- where the number of missing arguments is provided.
data PartCall = PartFuncCall Int | PartConsCall Int
 deriving (Show, Eq)

-- A graph node is a function, constructor, choice, or free node.
-- The latter represents unbound variables.
data Node = FuncNode   String   [NodeID]
          | ConsNode   String   [NodeID]
          | PartNode   String   PartCall [NodeID]
          | ChoiceNode ChoiceID NodeID NodeID
          | FreeNode
 deriving (Show, Eq)

-- The label of a node.
nodeLabel :: Node -> String
nodeLabel (FuncNode f _)       = f
nodeLabel (ConsNode f _)       = f
nodeLabel (PartNode f _ _)     = f
nodeLabel (ChoiceNode cid _ _) = "?" ++ show cid
nodeLabel FreeNode             = "free"

nodeChildren :: Node -> [NodeID]
nodeChildren (FuncNode _ cs)       = cs
nodeChildren (ConsNode _ cs)       = cs
nodeChildren (PartNode _ _ cs)     = cs
nodeChildren (ChoiceNode _ c1 c2) = [c1, c2]
nodeChildren FreeNode              = []

-- Add an argument node to a node representing a partial call.
addPartialArg :: Node -> NodeID -> Node
addPartialArg pnode arg = case pnode of
  PartNode fc (PartFuncCall m) args ->
    let nargs = args ++ [arg]
    in if m == 1
         then FuncNode fc nargs
         else PartNode fc (PartFuncCall (m-1)) nargs
  PartNode fc (PartConsCall m) args ->
    let nargs = args ++ [arg]
    in if m == 1
         then ConsNode fc nargs
         else PartNode fc (PartConsCall (m-1)) nargs
  _ -> error "addPartialArg: node does not contain partial call"


-- A graph is implemented as a list of nodes together with a maximum NodeID
-- and a current root (only necessary for visualization).
data Graph = Graph [(NodeID,Node)] NodeID NodeID
 deriving (Show, Eq)

-- An empty graph contains only a "null" node with node id 0.
emptyGraph :: Graph
emptyGraph = Graph [(0, ConsNode "null" [])] 1 1

-- Looks up a node in a graph (where redirections are considered).
lookupNode :: NodeID -> Graph -> Node
lookupNode ni (Graph nodes _ _) =
  maybe (error $ "Node with id `" ++ show ni ++ "` not found!")
        id
        (lookup ni nodes)

graphRoot :: Graph -> NodeID
graphRoot (Graph _ _ nid) = nid

-- Returns maximum node id of a graph.
maxNodeID :: Graph -> NodeID
maxNodeID (Graph _ m _) = m

-- Adds a new node in a graph.
addNode :: Node -> Graph -> (Graph,NodeID)
addNode node (Graph nodes mx root) =
  (Graph (nodes ++ [(mx,node)]) (mx + 1) root, mx)

-- Updates a node in a graph with new node information.
updateNode :: Graph -> NodeID -> Node -> Graph
updateNode (Graph nodes mx root) nid newnode = Graph (map update nodes) mx root
 where
  update (ni, n) | ni == nid = (ni, newnode)
                 | otherwise = (ni, n)

-- Replaces a node `n1` in a graph by another node `n2` already contained
-- in the graph.
-- Thus, all edges with target `n1` are redirected to target `n2`.
-- Furthermore, node `n1` is deleted from the graph since it is garbage.
replaceNode :: Graph -> NodeID -> NodeID -> Graph
replaceNode (Graph nodes mx root) oldnid newid =
  Graph (filter ((/= oldnid) . fst) . map redirect $ nodes) mx newroot
 where
  redirect (ni, n) = (ni, redirectTargets n)
  newroot = if root == oldnid then newid else root

  redirectTargets (FuncNode f ns)      = FuncNode f (map redirectTarget ns)
  redirectTargets (ConsNode f ns)      = ConsNode f (map redirectTarget ns)
  redirectTargets (PartNode f n ns)    = PartNode f n (map redirectTarget ns)
  redirectTargets (ChoiceNode c n1 n2) =
    ChoiceNode c (redirectTarget n1) (redirectTarget n2)
  redirectTargets FreeNode             = FreeNode

  redirectTarget ni = if ni == oldnid then newid else ni


-- Shows the expression represented by the graph starting with a given node.
-- In order to visualize sharing, shared subexpressions are shown
-- as let expressions.
showGraphExp :: Graph -> NodeID -> String
showGraphExp g nid = showExp [] 10 nid
 where
  showExp lets d ni | d == 0    = "..."
                    | otherwise = showNExp (lookupNode ni g)
   where
    showNExp FreeNode             = "_x" ++ show ni
    showNExp (ConsNode f args)    = showNExp (FuncNode f args)
    showNExp (PartNode f _ args)  = showNExp (FuncNode f args)
    showNExp (ChoiceNode c n1 n2) = showNExp (FuncNode ('?' : show c) [n1,n2])
    showNExp (FuncNode f args)
      | null args = f
      | otherwise
      = "(" ++
        (if null arglets
           then ""
           else "let {" ++
                intercalate " ; " (map showLetDecl arglets) ++ "} in ") ++
        showCall (map (\a -> if a `elem` alllets
                               then showVar a
                               else showExp alllets (d-1) a) args) ++
        ")"
     where
      showCall cargs =
        if isInfixOp f
          then case cargs of
                 [a1,a2] -> unwords [a1,f,a2]
                 _       -> unwords (('(' : f ++ ")") : cargs)
          else unwords (f : cargs)
       where
        isInfixOp = all (`elem` "!@#$%^&*+-=<>?./|\\:")

      arglets = nub (concatMap
                       (\a -> let occs = occursInGraphExp d a ni
                              in if occs < 2 || isConst a then [] else [a])
                       (filter (`notElem` lets) args))

      alllets = arglets ++ lets

      showLetDecl a = showVar a ++ " = " ++ showExp alllets (d-1) a

  showVar ni = 'x' : show ni

  isConst ni = case lookupNode ni g of
        ConsNode _ [] -> True
        _             -> False

  -- count occurrences of a node `ni` in graph rooted by `nr`
  occursInGraphExp d ni nr
    | d == 0 = 0
    | otherwise
    = (if ni==nr then 1 else 0) +
      case lookupNode nr g of
        FuncNode _   args  -> foldr (+) 0 (map (occursInGraphExp (d-1) ni) args)
        ConsNode _   args  -> foldr (+) 0 (map (occursInGraphExp (d-1) ni) args)
        PartNode _ _ args  -> foldr (+) 0 (map (occursInGraphExp (d-1) ni) args)
        ChoiceNode _ a1 a2 -> occursInGraphExp (d-1) ni a1 +
                              occursInGraphExp (d-1) ni a2
        FreeNode           -> 0


-- Transforms a graph (w.r.t. given node attributes) into a dot graph.
-- If the third argument is True, node ids will be shown in the node labels.
-- If the fourth argument is True, all nodes are shown, otherwise
-- only nodes reachable from the node list in the second argument are shown.
graphToDot :: Graph -> [(NodeID,[(String,String)])] -> Bool -> Bool -> DotGraph
graphToDot gr ndattrs withnids showall =
  let sgraph = if showall then gr
                          else reachableGraph gr (map fst ndattrs)
  in fullGraphToDot sgraph ndattrs withnids

-- Transforms a graph (w.r.t. given node attributes) into a dot graph.
-- If the third argument is True, node ids will be shown in the node labels.
fullGraphToDot :: Graph -> [(NodeID,[(String,String)])] -> Bool -> DotGraph
fullGraphToDot (Graph nodes _ _) ndattrs withnids =
  dgraph "Graph" (concatMap toNode nodes) (concatMap toEdges nodes)
 where
  toNode (nid,n) = [Dot.Node (show nid) ([("label", ndlabel)] ++ addAttrs)]
   where
    ndlabel  = nodeLabel n ++ if withnids then " [" ++ show nid ++ "]" else ""
    addAttrs = maybe [] id (lookup nid ndattrs)

  toEdges (nid, FuncNode _   ns   ) = addEdges nid ns
  toEdges (nid, ConsNode _   ns   ) = addEdges nid ns
  toEdges (nid, PartNode _ _ ns   ) = addEdges nid ns
  toEdges (nid, ChoiceNode _ n1 n2) = addEdges nid [n1,n2]
  toEdges (_  , FreeNode          ) = []

  addEdges src ns =
    map (\ (i,n) -> Dot.Edge (show src) (show n) [("label",show i)])
        (zip [1..] ns)

-- "Garbage collection" on a graph: remove all nodes (and their outgoing edges)
-- which are not reachable from a given list of nodes.
reachableGraph :: Graph -> [NodeID] -> Graph
reachableGraph (Graph nodes mx rt) initnids =
  let rnodes = reachableNodes initnids
  in Graph (filter ((`elem` rnodes) . fst) nodes) mx rt
 where
  reachableNodes nids =
     let newts = nub . filter (`notElem` nids) . concatMap targets $ nids
     in if null newts then nids else reachableNodes (newts ++ nids)
   where
    targets ni =
      maybe (error $ "Node with id `" ++ show ni ++ "` not found!")
            (\nd -> case nd of FuncNode _ ns      -> ns
                               ConsNode _ ns      -> ns
                               PartNode _ _ ns    -> ns
                               ChoiceNode _ n1 n2 -> [n1,n2]
                               FreeNode           -> [])
            (lookup ni nodes)

------------------------------------------------------------------------------
