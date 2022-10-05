------------------------------------------------------------------------------
--- Operations to transform ICurry graphs into SVG representation.
---
--- @author Sascha Ecks
--- @version September 2022
------------------------------------------------------------------------------

module TermGraph.SVG --(xmlGraphs2Svgs, xmlGraphs2SvgGraphs, main)
 where

import XML
import Data.Maybe         (fromMaybe)
import Data.List          (find, sum, maximum, minimum)
import qualified Data.Map as M
import System.Environment (getArgs)
import Control.Monad      (when)
import TermGraph.XML      ( State(..), replaceChoiceIDs )
import qualified ICurry.Graph as IG

type Graph = [Node]

type NodeID = Int

type ChoiceMapping = (NodeID,Int)

--               Type     ID     Label  Children Active Result
data Node = Node NodeType NodeID String [NodeID] Bool   Bool
 deriving Show

getNodeId :: Node -> NodeID
getNodeId (Node _ nid _ _ _ _) = nid

getNodeChildren :: Node -> [NodeID]
getNodeChildren (Node _ _ _ chl _ _) = chl


-- Node type representing a node/tree for visualisation
data DNode = DNode
  { nodeType    :: NodeType,
    nodeID      :: NodeID,
    label       :: String,
    children    :: [DNode],
    strokeColour :: String,
    fillColour  :: String,
    width       :: Int --total width of this node's (sub-)graph in Nodes
  }
 deriving Show

instance Eq DNode where
  node1 == node2 = (nodeID node1) == (nodeID node2)

-- Node type representing a node for (graph-)visualisation
data DGNode = DGNode {
                  nodeTypeDG     :: NodeType,
                  nodeIDDG       :: NodeID,
                  labelDG        :: String,
                  childrenDG     :: [NodeID],
                  strokeColourDG :: String,
                  fillColourDG   :: String,
                  depthDG        :: Int
                 }
 deriving Show

type DGGraph = [DGNode]

data NodeType = FuncNode | ConsNode | ChoiceNode | FreeNode
 deriving (Show, Read)


main :: IO ()
main = do
    args <- getArgs
    when (length args < 1 || length args > 4) $ error
      "Too many/few arguments. Provide between one and four arguments"
    let (inFile, outFile, graphSvgFunc) = processArgs args
    xmlGraphs <- readXmlFile inFile
    graphSvgFunc outFile xmlGraphs

processArgs :: [String] -> (FilePath, FilePath, (FilePath -> XmlExp -> IO ()))
processArgs args =
  let dims = read <$> (args !? 3)
  in (args !! 0,
      if length args > 1
        then args !! 1
        else "icurrySVG",
      if args !? 2 == (Just "-tree")
        then (xmlGraphs2Svgs False (treeSvg 10 dims))
        else (xmlGraphs2Svgs False (graphSvg dims)))


type DrawMode = (Graph, [ChoiceMapping], NodeID) -> XmlExp

graphs2Svgs :: Bool -> DrawMode -> FilePath -> [State] -> IO ()
graphs2Svgs labelNID draw outFile states = do
    let graphs = map (convertState labelNID) states
    let svgs = map draw graphs
    writeGraphSvgs outFile 0 svgs

-- Convert a xml graphlist to svgs for each graph and write them
xmlGraphs2Svgs :: Bool -> DrawMode -> FilePath -> XmlExp -> IO ()
xmlGraphs2Svgs labelNID draw outFile xmlGraph = do
    let graphs = readXmlGraphList labelNID xmlGraph
    let svgs = map draw graphs
    writeGraphSvgs outFile 0 svgs

-- Write a list of svgs and append indexes to them
writeGraphSvgs :: FilePath -> Int -> [XmlExp] -> IO ()
writeGraphSvgs _ _ [] = return ()
writeGraphSvgs file index (svg:svgs) = do
  writeXmlFile (file ++ (show index) ++ ".svg") svg
  writeGraphSvgs file (index + 1) svgs


-- Calculate a color map for all shared nodes (i.e. nodes that are children
-- of multiple nodes)
-- Note that the root node is always already visited.
sharingColorMap :: Graph -> [NodeID] -> [(NodeID, String)] -> Int
                -> [(NodeID, String)]
sharingColorMap [] _ cmap _ = cmap
sharingColorMap (node:ns) visChld cmap cInd =
  uncurry3 (sharingColorMap ns)
           (checkChildren (getNodeChildren node) visChld cmap cInd)
 where
  checkChildren [] visChld' cmap' cInd' = (visChld', cmap', cInd')
  checkChildren (c:cs) visChld' cmap' cInd'
    | any ((== c) . fst) cmap' = checkChildren cs visChld' cmap' cInd'
    -- first guard implies this case, its condition will
    -- be false if this gets reached
    | elem c visChld'  = checkChildren
                                  cs
                                  visChld'
                                  ((c, shareColors !! cInd') : cmap')
                                  ((cInd' + 1) `mod` (length shareColors))
    | otherwise       = checkChildren cs (c:visChld') cmap' cInd'

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- List of colors to be used when marking shared nodes
shareColors :: [String]
shareColors = ["red", "blue", "magenta", "orange", "purple", "darkgreen",
                "saddlebrown", "cyan", "darkred", "olive", "pink"]

-- Calculate a node's fill color from its active and result flags
calcFillColor :: Bool -> Bool -> String
calcFillColor active result = case active of
      True -> "yellow"
      False -> case result of
        True -> "lime"
        False -> "white"

-- Convert states from program execution to states for svg creation
convertState :: Bool -> State -> (Graph, [ChoiceMapping], NodeID)
convertState labelNID state = (g, choiceMappings, root)
  where
    IG.Graph nodes _ root = graph state
    choiceMappings = replaceChoiceIDs nodes (fingerprint state)
    g = map (generateNode labelNID (activeNode state) (results state)) nodes

generateNode :: Bool -> NodeID -> [NodeID] -> (IG.NodeID, IG.Node) -> Node
generateNode labelNID activenid results (nid, node) =
    Node
      nodetype
      nid
      label
      (IG.nodeChildren node)
      (activenid == nid)
      (nid `elem` results)
        where
          label = (IG.nodeLabel node) ++ if labelNID
                                          then " (" ++ (show nid) ++ ")"
                                          else ""
          nodetype = case node of
            IG.FuncNode _ _     -> FuncNode
            IG.ConsNode _ _     -> ConsNode
            IG.ChoiceNode _ _ _ -> ChoiceNode
            IG.FreeNode         -> FreeNode
            IG.PartNode _ (IG.PartFuncCall _) _ -> FuncNode
            IG.PartNode _ (IG.PartConsCall _) _ -> ConsNode

readXmlGraphList :: Bool -> XmlExp -> [(Graph, [ChoiceMapping], NodeID)]
readXmlGraphList labelNID xml = let graphs = filterElems "graph" (elemsOf xml)
                                 in zip3 (map (graphFromXml labelNID) graphs)
                                         (map choiceMappingsFromXml graphs)
                                         (map rootFromXml graphs)

-- Extract a Graphs from a xml graph
graphFromXml :: Bool -> XmlExp -> Graph
graphFromXml labelNID xml = case xml of
  XElem "graph" _ listelems -> map (nodeFromXml labelNID)
                                   (filterElems "node" listelems)
  _                         -> error "No gaph supplied"

-- Extract a node from Xml representation
nodeFromXml :: Bool -> XmlExp -> Node
nodeFromXml labelNID (XElem nodetype _ attrs)
  | nodetype == "node" = Node (readElem attrs "type")
                              (readElem attrs "id")
                              label
                              children
                              (readElem attrs "isActive")
                              (readElem attrs "isResult")
  | otherwise          = error "could not parse node"
 where
  label = readStrElem attrs "label" ++
          if labelNID then " (" ++ (readStrElem attrs "id") ++ ")"
                      else ""

  childrenXml :: [XmlExp]
  childrenXml = ((elemsOf . (fromMaybe (error "Error in nodeFromXml")) . find ((== "children") . tagOf)) attrs)

  children    :: [NodeID]
  children    = map (read . textOfXmlExp)
                    (filter ((== "nodeId") . tagOf) childrenXml)
nodeFromXml _ (XText _) = error "could not parse node"

rootFromXml :: XmlExp -> NodeID
rootFromXml xml = case xml of
            XElem "graph" _ attrs -> readElem attrs "root"
            _                     -> error "No graph supplied"

-- Extract choice mappings from a single xml-graph
choiceMappingsFromXml :: XmlExp -> [ChoiceMapping]
choiceMappingsFromXml xml = case xml of
            XElem "graph" _ attrs ->
              let xmlCMs = filterElems "ChoiceMapping" attrs
              in map (\x -> (getAttribute "from" x, (getAttribute "to" x) - 1))
                     xmlCMs
            _                     -> error "No graph supplied"

-- get the Value of a spcific Attribute from an Xml Expression
getAttribute :: Read a => String -> XmlExp -> a
getAttribute attr (XElem _ attrs _) = (read . (fromMaybe (error "Error in getAttribute")) . (lookup attr)) attrs
getAttribute _    (XText _)         = error "XText does not contain attributes"

-- filter all Xml elements with a given tag from a list of Xml elements
filterElems :: String -> [XmlExp] -> [XmlExp]
filterElems tag elems = ((filter ((== tag) . tagOf)) elems)

--Extract the Text of a Xml element with a given tag from a list of Xml elements
readElem :: Read a => [XmlExp] -> String -> a
readElem xmlexps attr =
    (read . textOfXmlExp . (fromMaybe (error "Error in readElem")) . (find ((== attr) . tagOf))) xmlexps

-- Because read does not work on Strings (as Strings are only Lists of Charaters)
readStrElem :: [XmlExp] -> String -> String
readStrElem xmlexps attr =
    (textOfXmlExp . (fromMaybe (error "Error in readStrElem")) . (find ((== attr) . tagOf))) xmlexps

textOfXmlExp :: XmlExp -> String
textOfXmlExp (XText s) = s
textOfXmlExp (XElem _ _ xs) = textOf xs

-- Get a node by NodeID from a graph
findNode :: Graph -> NodeID -> Node
findNode graph nid =
  let mnode = find ((== nid) . getNodeId) graph
  in case mnode of
      Nothing   -> error ("Invalid NodeID in graph" ++ (show nid))
      Just node -> node

findDGNode :: DGGraph -> NodeID -> DGNode
findDGNode graph nid =
  let mnode = find ((== nid) . nodeIDDG) graph
  in case mnode of
       Nothing   -> error ("Invalid NodeID in DG-graph: " ++ show nid ++
                           "\n" ++ show graph)
       Just node -> node

class GraphMetric a where
  nodeChildrenById :: [a] -> NodeID -> [NodeID]

instance GraphMetric Node where
  nodeChildrenById graph node = getNodeChildren (findNode graph node)

instance GraphMetric DGNode where
  nodeChildrenById graph node = childrenDG (findDGNode graph node)

-- Operation modes for graphMetric to calculate width or depth of a graph
modeWidth :: (Int -> Int -> Int, Int -> Int)
modeWidth = ((+), id)

modeDepth :: (Int -> Int -> Int, Int -> Int)
modeDepth = (max, (+1))

graphDepth :: GraphMetric n => [n] -> NodeID -> Int
graphDepth graph start = fst $ graphMetric modeDepth [] graph start

graphWidth :: GraphMetric n => [n] -> NodeID -> Int
graphWidth graph start = fst $ graphMetric modeWidth [] graph start

-- calculate depth or width of a given graph. Also return a list of visited Nodes
graphMetric :: GraphMetric n => (Int -> Int -> Int, Int -> Int) -> [NodeID]
            -> [n] -> NodeID -> (Int, [NodeID])
graphMetric (aggr, incr) visited graph curr =
  let children = nodeChildrenById graph curr
  in case children of
    []  -> (1, curr : visited)
    _:_ | elem curr visited -> (1, visited)
        | otherwise -> incDepth (summarize (curr : visited) children)
 where
  incDepth (d,v)       = (incr d,v)
  summarize vis []     = (0, vis)
  summarize vis (c:cs) =
    let (currsum, currvis) = graphMetric (aggr, incr) vis graph c
        (nextsum, nextvis) = summarize currvis cs
    in (aggr currsum nextsum, nextvis)

--Construct a graph for drawing from a graph
constructDGGraph :: Graph -> NodeID -> DGGraph
constructDGGraph graph root =
  constructDGGraph' (constructPredMap graph) M.empty graph root

constructDGGraph' :: [(NodeID, [NodeID])] -> M.Map NodeID Int -> Graph
                  -> NodeID -> DGGraph
constructDGGraph' _       _        []           _    = []
constructDGGraph' predMap depthMap (node:nodes) root =
  (newDGNode node) : (constructDGGraph' predMap newDepthMap nodes root)
 where
  newDepthMap = getNodeDepth predMap depthMap [] root (getNodeId node)
  depth       = fromMaybe (error "Error in getNodeDepth") $
                  M.lookup (getNodeId node) newDepthMap
  newDGNode (Node ntype nid label chld ac res) =
    DGNode ntype nid label chld "black" (calcFillColor ac res) depth

-- Build a Map from NodeID to the corresponding node's predecessors' nodeIDs.
constructPredMap :: [Node] -> [(NodeID, [NodeID])]
constructPredMap []           = []
constructPredMap (node:nodes) =
  insertPreds (constructPredMap nodes) currChildren
 where currChildren = getNodeChildren node
       insertPreds = foldl (insertMapKey (getNodeId node))
       insertMapKey val m key = case lookup key m of
         Nothing   -> (key, [val]) : m
         Just list -> (key, (val : list)) : (filter ((/= key) . fst) m)


-- calculate the depth a node needs to be drawn at:
-- Normally it's the length of the longest (non-cyclic) path it can be reached by,
-- if a node has multple direct predecessors, add one for better visuals
getNodeDepth :: [(NodeID, [NodeID])] -> M.Map NodeID Int -> [NodeID]
                    -> NodeID -> NodeID ->  M.Map NodeID Int
getNodeDepth predMap depthMap visited root currNode =
    case (M.lookup currNode depthMap) of
      Just _  -> depthMap
      Nothing
        | currNode `elem` visited -> depthMap
        | currNode == root -> M.insert currNode 0 depthMap
        | otherwise -> case maxPath of
                        Nothing -> newMap
                        Just mp -> M.insert
                                    currNode
                                    (if length preds > 1 then mp + 2
                                                         else mp + 1)
                                    newMap
            where
              preds   = filter (/= currNode)
                               (fromMaybe
                                  (error "Error in getNodeDepth, could not find node")
                                  (lookup currNode predMap))
              maxPath = maximum (map ((flip M.lookup) newMap) preds)
              newMap  = foldr (\pr m -> getNodeDepth predMap m (currNode : visited) root pr) depthMap preds


--Construct a linked tree for drawing from an unlinked graph
--returns the tree and its depth
constructDGraph :: Int -> Graph -> NodeID -> (DNode, Int)
constructDGraph maxDepth graph root = (dGraph, min (1 + maxDepth - reached) maxDepth)
  where
    (dGraph, reached) = constructDGraph' graph (sharingColorMap graph [root] [] 0) maxDepth root

constructDGraph' :: Graph -> [(NodeID,String)] -> Int -> NodeID -> (DNode, Int)
constructDGraph' graph cmap maxDepth curr =
  (newDNode currNode, if length depths == 0 then maxDepth else minimum depths)
 where
  currNode = findNode graph curr
  (children, depths) = if maxDepth <= (-1) -- render one more level which isn't fully rendered
              then ([], [])                -- this can show that the tree would not end there
              else unzip $ map (constructDGraph' graph cmap (maxDepth - 1)) (getNodeChildren currNode)
  nodeWidth = max 1 (sum (map width children))
  newDNode (Node ntype nid label _ ac res) =
                            DNode ntype
                                  nid
                                  label
                                  children
                                  (((fromMaybe "black") . (lookup nid)) cmap)
                                  (calcFillColor ac res)
                                  nodeWidth


type Point = (Float,Float)
type Dimensions = Point

-- The smallest size of a node width, height
nodeSize :: Dimensions
nodeSize = (40,30)

calcNodeWidth :: String -> Float
calcNodeWidth lbl = max ((toFloat (length lbl)) * 9) (fst nodeSize)



-- Draw a svg-graph of a given graph
graphSvg :: Maybe Dimensions -> (Graph, [ChoiceMapping], NodeID) -> XmlExp
graphSvg dims (graph, chMap, root) =
  drawSvg dims (constructDGGraph graph root) root chMap

-- Draw a svg of a given draw-Graph
drawSvg :: Maybe Dimensions -> DGGraph -> NodeID -> [ChoiceMapping] -> XmlExp
drawSvg dims drawGraph root chMap =
  let deepestNd = filter ((==(maximum (map depthDG drawGraph))) . depthDG)
                         drawGraph
      dgHeight  = toFloat $ depthDG (head deepestNd) +
                    if length (concatMap childrenDG deepestNd) > 0
                      then 2
                      else 1
      dgWidth   = toFloat (graphWidth drawGraph root)
      calcX     = dgWidth * 140
      calcY     = max (dgHeight * 80) 400.0
      dimX      = fromMaybe (max calcX calcY) (fst <$> dims)
      dimY      = fromMaybe dimX (snd <$> dims)
  in XElem "svg" [("viewbox", "0 0 " ++ show dimX ++ " " ++ show dimY),
                  ("xmlns", "http://www.w3.org/2000/svg")]
               (fst (graphSvgRek
                  (dimY / dgHeight)
                  (dimX / dgWidth)
                  0
                  root
                  drawGraph
                  chMap
                  []))

graphSvgRek :: Float -> Float -> Float -> NodeID -> DGGraph -> [ChoiceMapping]
                -> [(NodeID, Point)] -> ([XmlExp], [(NodeID, Point)])
graphSvgRek   levelHeight leafWidth leftBound currNID graph chMap drawnNodes
    | elem currNID (fst (unzip drawnNodes)) = ([], drawnNodes)
    | otherwise = ((dgNodeSvg currNode (posX,posY)) : children, drawnNodes')
          where
            (children, drawnNodes') = (childrenSvg currChldrn leftBound
                                        (currChldrn !?? ((-1 +) <$> (lookup currNID chMap)))
                                        ((currNID, (posX,posY)) : drawnNodes))
            currNode       = findDGNode graph currNID
            currChldrn     = childrenDG currNode
            currGraphWidth = fst (graphMetric modeWidth (map fst drawnNodes) graph currNID)
            posY = ((toFloat (depthDG currNode)) + 0.5) * levelHeight
            posX = leftBound + ((toFloat currGraphWidth) * leafWidth / 2)
            childrenSvg :: [NodeID] -> Float -> Maybe NodeID -> [(NodeID, Point)] -> ([XmlExp], [(NodeID, Point)])
            childrenSvg []     _        _          pMap = ([], pMap)
            childrenSvg (c:cs) currLeft choiceChld pMap =
              let --TODO: add graphWidth as argument to GraphSvgRek to reduce calculations of it
                  cWidth = fst (graphMetric modeWidth (map fst pMap) graph c)
                  (subG, newpMap) = graphSvgRek levelHeight leafWidth currLeft c graph chMap pMap
                  (nextChildrn, newpMap') = (childrenSvg cs (currLeft + (toFloat cWidth) * leafWidth) choiceChld newpMap)
                  currChld = findDGNode graph c
              in ((connection currNode currChld currLeft levelHeight leafWidth ((Just c) == choiceChld) newpMap' cWidth)
                  : (subG ++ nextChildrn), newpMap')

            --draw a connection between the current node and the current child
            connection :: DGNode -> DGNode -> Float -> Float -> Float -> Bool -> [(NodeID, Point)] -> Int -> XmlExp
            connection currNode c currLeft levelHeight leafWidth thick pMap cWidth =
              let midX     = (currLeft + (toFloat cWidth) * leafWidth / 2)
                  --TODO: move transX if child and parent are on the same x-coord!
                  transX   = if chldLvl > startLvl
                              then midX
                              else midX + (absMax ((chldX - midX) * 0.4) 40)
                  startLvl = depthDG currNode
                  chldLvl  = depthDG c
                  transTo  = if chldLvl > startLvl then chldLvl-1 else chldLvl
                  sWidth   = if thick then 3 else 1
                  chldShort = if (chldLvl /= startLvl + 1) then 0 else (snd nodeSize)/2
                  (posX, posY) = fromMaybe  (error "Error in connection, couldnt find position")
                                            (lookup (nodeIDDG currNode) pMap)
                  (chldX, chldY) = fromMaybe  (error "Error in connection, couldnt find position")
                                              (lookup (nodeIDDG c) pMap)
              in XElem "g" [("stroke-width", show sWidth)]
                -- curve to the regular position of child node
                ([ (bezierSvg
                    (posX, posY + ((snd nodeSize) / 2))
                    (midX, ((toFloat startLvl) + 1.5) * levelHeight - chldShort)
                    sWidth) ] ++
                -- bow for connection to node on higher level
                ( condList (chldLvl <= startLvl)
                    [ (bezBowSvg
                        (midX, ((toFloat startLvl) + 1.5) * levelHeight)
                        (transX, ((toFloat startLvl) + 1.5) * levelHeight)
                        1) ] ) ++
                -- straight line over n levels
                ( condList (chldLvl /= startLvl + 1)
                    [ (nLevelConnection transX (startLvl+1) transTo sWidth) ] ) ++
                -- curve to actual position of node on lower level
                ( condList (chldLvl > startLvl + 1)
                    [ (bezierSvg
                        (transX, ((toFloat chldLvl) - 0.5) * levelHeight)
                        (chldX, chldY - (snd nodeSize)/2)
                        sWidth) ] ) ++
                -- bow to actual position of node on higher level
                ( condList (chldLvl <= startLvl)
                    [ (bezBowSvg
                        (transX, ((toFloat chldLvl) + 0.5) * levelHeight)
                        (chldX, ((toFloat chldLvl) + 0.5) * levelHeight - (snd nodeSize) / 2)
                        (-1) ) ] ) )

            nLevelConnection :: Float -> Int -> Int -> Int -> XmlExp
            nLevelConnection posX startLevel endLevel sWidth
                    | startLevel == endLevel = XText ""
                    | otherwise              = XElem "line"
                                [ ( "x1", show posX ),
                                  ( "y1", show (((toFloat startLevel) + 0.5) * levelHeight) ),
                                  ( "x2", show posX ),
                                  ( "y2", show (((toFloat endLevel) + 0.5) * levelHeight) ),
                                  ( "stroke", "black" ),
                                  ( "stroke-width", (show sWidth) ) ]
                                []

absMax :: (Ord a, Num a) => a -> a -> a
absMax x y = if (abs x) > (abs y) then x else y

-- Draw a svg-tree of a given graph
treeSvg :: Int -> Maybe Dimensions -> (Graph, [ChoiceMapping], NodeID) -> XmlExp
treeSvg maxDepth dims (graph, chMap, root) =
  let (drawGraph, depth) = constructDGraph maxDepth graph root
      calcX      = toFloat ((width drawGraph) * 140)
      calcY      = max (toFloat depth * 80) 400.0
      dimX       = fromMaybe (max calcX calcY) (fst <$> dims)
      dimY       = fromMaybe dimX (snd <$> dims)
  in XElem "svg" [("viewbox", "0 0 " ++ show dimX ++ " " ++ show dimY),
                  ("xmlns", "http://www.w3.org/2000/svg")]
           (treeSvgRek
              maxDepth
              0
              (dimY / (toFloat depth))
              (dimX / (toFloat $ width drawGraph))
              0
              drawGraph
              chMap)


treeSvgRek :: Int -> Int -> Float -> Float -> Float -> DNode -> [ChoiceMapping]
           -> [XmlExp]
treeSvgRek maxDepth level levelHeight leafWidth leftBound currNode chMap =
  (nodeSvg currNode (posX,posY)) :
  (childrenSvg currChldrn leftBound
               (currChldrn !?? ((-1 +) <$> (lookup (nodeID currNode) chMap))))
 where
   currChldrn = children currNode
   posX = leftBound + ((toFloat $ width currNode) * leafWidth / 2)
   posY = ((toFloat level) + 0.5) * levelHeight
   connectionDashes = if level >= maxDepth - 1 then "5,5" else ""
   connectionColour = if level >= maxDepth - 1 then "darkgrey" else "black"

   childrenSvg []     _        _          = []
   childrenSvg (c:cs) currLeft choiceChld =
     (connection c currLeft ((Just c) == choiceChld)) :
     (treeSvgRek maxDepth (level + 1) levelHeight leafWidth currLeft c chMap) ++
     (childrenSvg cs (currLeft + (toFloat $ width c) * leafWidth) choiceChld)

   --draw a connection between the current node and the current child
   connection c currLeft thick =
     dashedBezierSvg connectionDashes connectionColour
       (posX, posY + ((snd nodeSize) / 2))
       (currLeft + (toFloat (width c)) * leafWidth / 2,
        ((toFloat level) + 1.5) * levelHeight - (snd nodeSize) / 2)
       (if thick then 3 else 1)


-- Return the given list if p is true else return the empty list
condList :: Bool -> [a] -> [a]
condList p list = if p then list else []

-- Same as (!?) but the index can be Nothing too.
(!??) :: [a] -> Maybe Int -> Maybe a
_  !?? Nothing  = Nothing
xs !?? (Just i) = xs !? i


-- Get index of a list and return it, if it exists. Return Nothing if not.
(!?) :: [a] -> Int -> Maybe a
[]     !? _ = Nothing
(x:xs) !? i | i >  0 =  xs !? (i-1)
            | i == 0 = Just x
            | otherwise = Nothing

-- draw a single node in svg-format
dgNodeSvg :: DGNode -> Point -> XmlExp
dgNodeSvg node p = let
          sColour = strokeColourDG node
          fColour = fillColourDG node
          nlabel  = cutLabel (labelDG node)
          nid     = nodeIDDG node
          width   = calcNodeWidth nlabel
        in case (nodeTypeDG node) of
          FuncNode   -> funcNodeSvg nlabel sColour fColour nid p width
          ConsNode  -> constNodeSvg nlabel sColour fColour nid p width
          ChoiceNode -> funcNodeSvg nlabel sColour fColour nid p width
          FreeNode   -> funcNodeSvg nlabel sColour fColour nid p width

-- draw a single node in svg-format
nodeSvg :: DNode -> Point -> XmlExp
nodeSvg node p = let
          sColour = strokeColour node
          fColour = fillColour node
          nlabel  = cutLabel (label node)
          nid     = nodeID node
          width   = calcNodeWidth nlabel
        in case (nodeType node) of
          FuncNode   -> funcNodeSvg nlabel sColour fColour nid p width
          ConsNode  -> constNodeSvg nlabel sColour fColour nid p width
          ChoiceNode -> funcNodeSvg nlabel sColour fColour nid p width
          FreeNode   -> funcNodeSvg nlabel sColour fColour nid p width

-----------------------------------------------------
-- A set of functions for drawing svg nodes and edges
-----------------------------------------------------

cutLabel :: String -> String
cutLabel = take 20

funcNodeSvg :: String -> String -> String -> NodeID -> Point -> Float -> XmlExp
funcNodeSvg label sColour fColour nid p width = XElem "g" [("class", "node" ++ (show nid))] [
                            (XElem "ellipse" (funcNodeAttrList sColour fColour p width) []),
                            (nodeTextSvg label p)]

funcNodeAttrList :: String -> String -> Point -> Float -> [(String,String)]
funcNodeAttrList sColour fColour (x,y) width = [("cx",(show x)),
                                  ("cy",(show y)),
                                  ("rx",(show $ (width) / 2)),
                                  ("ry",(show $ (snd nodeSize) / 2))] ++
                                  (colourAttrList sColour fColour)

constNodeSvg :: String -> String -> String -> NodeID -> Point -> Float -> XmlExp
constNodeSvg label sColour fColour nid p width = XElem "g" [("class", "node" ++ (show nid))] [
                            (XElem "rect" (constNodeAttrList sColour fColour p width) []),
                            (nodeTextSvg label p)]

constNodeAttrList :: String -> String -> Point -> Float -> [(String,String)]
constNodeAttrList sColour fColour (x,y) width = [("x",show $ x - (width / 2)),
                                  ("y",show $ y - ((snd nodeSize) / 2)),
                                  ("width",(show $ width)),
                                  ("height",(show $ snd nodeSize))] ++
                                  (colourAttrList sColour fColour)

colourAttrList :: String -> String -> [(String,String)]
colourAttrList sColour fColour =
      [ ("fill",fColour),
        ("stroke",sColour),
        ("stroke-width",if sColour /= "black"
          then "3"
          else "1") ]

-- place text inside a svg-node TODO: include text-length
nodeTextSvg :: String -> Point -> XmlExp
nodeTextSvg label (x,y) = XElem "text" [("x",(show x)),
                                          ("y",(show (y+5))),
                                          ("text-anchor","middle"),
                                          ("font-size","12")]
                                        [XText label]


bezierSvg :: Point -> Point -> Int -> XmlExp
bezierSvg from to sWidth = dashedBezierSvg "" "black" from to sWidth

dashedBezierSvg :: String -> String -> Point -> Point -> Int -> XmlExp
dashedBezierSvg dashes colour from to sWidth = let bOffset = ((snd to) - (snd from)) * 0.2
                            in XElem "path"
                                  [ ( "d", unwords ["M", (svgCoord from),
                                          "C", (svgCoord (movePoint from (0, bOffset))),
                                          (svgCoord (movePoint to (0, -bOffset))),
                                          (svgCoord to)] ),
                                    ( "stroke", colour ),
                                    ( "stroke-width", (show sWidth) ),
                                    ( "fill", "none" ),
                                    ( "stroke-dasharray", dashes ) ]
                                  []

-- draws a 180° turn Bezier curve
bezBowSvg :: Point -> Point -> Float -> XmlExp
bezBowSvg from to dir = let bOffset = dir * (snd nodeSize) * 0.7
                  in XElem "path"
                        [ ( "d", unwords ["M", (svgCoord from),
                                "C", (svgCoord (movePoint from (0, bOffset))),
                                (svgCoord (movePoint to (0, bOffset))),
                                (svgCoord to)] ),
                          ( "stroke", "black" ),
                          ( "fill", "none" ) ]
                        []

movePoint :: Point -> (Float,Float) -> Point
movePoint (x,y) (dx,dy) = (x+dx,y+dy)

svgCoord :: Point -> String
svgCoord (x,y) = (show x) ++ "," ++ (show y)
