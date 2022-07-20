------------------------------------------------------------------------------
--- Operations to transform ICurry graphs into XML representation.
---
--- @author Sascha Ecks
--- @version July 2022
------------------------------------------------------------------------------

module TermGraph.XML
 where

import ICurry.Graph
import XML
import Data.Maybe   (fromJust)
import Data.List    (nub)

-- The finger print is a partial mapping from choice identifiers to integers.
type FingerPrint = [(ChoiceID,Int)]

-- Representation of an execution state's information relevant for graph drawing
data State = State  { graph       :: Graph
                    , activeNode  :: NodeID
                    , results     :: [NodeID]
                    , fingerprint :: FingerPrint
                    }
 deriving Show

generateTextgraph :: State -> String
generateTextgraph = showXmlDoc . generateXmlgraph

compose4 :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
compose4 = (.) . (.) . (.) . (.)

-- generate a xml-graphlist from a list of reduced execution states
states2XmlGraphs :: [State] -> XmlExp
states2XmlGraphs states = xml "graphlist" (map generateXmlgraph states)

-- generate a xml representation of a termgraph by generating one
-- for every note of the graph
generateXmlgraph :: State -> XmlExp
generateXmlgraph (State (Graph nodes _ root) activenid results fp) = xml "graph" $
    (xmlEntry "root" (show root)) :
    (map (generateXmlNode activenid results) nodes) ++
    (map xmlChoiceMapping (replaceChoiceIDs nodes fp))

-- generate a xml representation of a graph node that contains
-- attributes relevant for drawing the graph
generateXmlNode :: NodeID -> [NodeID] -> (NodeID, Node) -> XmlExp
generateXmlNode activenid results (nid, node) =
                XElem "node" []
                [
                  xmlEntry "id" (show nid),
                  xmlEntry "type" nType,
                  xmlEntry "label" label,
                  xmlEntry "isActive" (show isActive),
                  xmlEntry "isResult" (show isResult),
                  XElem "children" []
                    (map ((xmlEntry "nodeId") . show) succs)
                ]

                where
                  (nType, label, succs) = case node of
                    FuncNode nm chld            -> ("FuncNode", nm, chld)
                    ConsNode nm chld            -> ("ConsNode", nm, chld)
                    ChoiceNode cid chld1 chld2  -> ("ChoiceNode", "? " ++ show cid, [chld1, chld2])
                    FreeNode                    -> ("FreeNode", "free", [])
                    PartNode nm (PartFuncCall _) chld -> ("FuncNode", nm, chld)
                    PartNode nm (PartConsCall _) chld -> ("ConsNode", nm, chld)
                  isActive = nid == activenid
                  isResult = elem nid results


xmlChoiceMappings :: [(NodeID,Int)] -> [XmlExp]
xmlChoiceMappings mappings = map xmlChoiceMapping mappings

xmlChoiceMapping :: (NodeID,Int) -> XmlExp
xmlChoiceMapping (nid,chld) = XElem "ChoiceMapping" [("from",show nid), ("to",show chld)] []

-- replace the ChoiceIDs in a given fingeprint with actual NodeIDs from a given graph
replaceChoiceIDs :: [(NodeID,Node)] -> FingerPrint -> [(NodeID,Int)]
replaceChoiceIDs _     []           = []
replaceChoiceIDs nodes ((cn,cm):ms) = map (\x -> (x,cm))
                                           (filter ((flip (choiceReachableFrom nodes ((cn,cm):ms) [])) 1)
                                                    (lookupChoiceNodeIds nodes cn))
                                      ++ (replaceChoiceIDs nodes ms)

-- lookup a ChoiceNodes NodeIDs.
lookupChoiceNodeIds :: [(NodeID,Node)] -> ChoiceID -> [NodeID]
lookupChoiceNodeIds []        _   = []
lookupChoiceNodeIds ((nid,n):nodes) cid = case n of
                            -- explicit case expressions are rigid (and deterministic)!
                            ChoiceNode ccid _ _  | ccid == cid -> nid : (lookupChoiceNodeIds nodes cid)
                                                 | otherwise   -> lookupChoiceNodeIds nodes cid
                            _                    -> lookupChoiceNodeIds nodes cid

--Function isn't very efficient, recursive application on children needs to be folded
--so that visited nodes get carried over.
choiceReachableFrom :: [(NodeID,Node)] -> FingerPrint -> [NodeID] -> NodeID -> NodeID -> Bool
choiceReachableFrom graph cms visited toNId fromNId
    | fromNId == toNId = True
    | otherwise        = let node = fromJust $ lookup fromNId graph
                             newvis = fromNId : visited
                          in case node of
                            ChoiceNode cid c1 c2
                              | fromJust (lookup cid cms) == 1 -> choiceReachableFrom graph cms newvis toNId c1
                              | fromJust (lookup cid cms) == 2 -> choiceReachableFrom graph cms newvis toNId c2
                            _ -> or $ map (choiceReachableFrom graph cms newvis toNId) (nub $ nodeChildren node)

xmlEntry :: String -> String -> XmlExp
xmlEntry attr val = xml attr [xtxt val]
