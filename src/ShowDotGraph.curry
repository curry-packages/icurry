--------------------------------------------------------------------------
--- A simple library for graph visualization with
--- [Graphviz](http://www.graphviz.org/).
--- It provides a data structure to represent graphs and operations
--- to visualize them.
---
--- @author Michael Hanus
--- @version December 2020
--------------------------------------------------------------------------

module ShowDotGraph
  ( DotGraph, dgraph, ugraph, Node(..), Edge(..)
  , viewDotGraph, showDotGraph, showDotGraphWithAttrs
  , getDotViewCmd, setDotViewCmd )
 where

import Char         ( isAlphaNum )
import Distribution ( rcFileName )
import IO
import IOExts
import List         ( intercalate, last )

import Data.PropertyFile ( getPropertyFromFile, updatePropertyFile )

--------------------------------------------------------------------------
-- Data types for graphs.

--- A Dot graph consists of a name and a list of nodes and edges.
--- It can be either directed (`DGraph`) or undirected (`UGraph`).
data DotGraph = DGraph String [Node] [Edge]
              | UGraph String [Node] [Edge]

--- Constructs a directed graph from a name and a list of nodes and edges.
dgraph :: String -> [Node] -> [Edge] -> DotGraph
dgraph name nodes edges = DGraph name nodes edges

--- Constructs an undirected graph from a name and a list of nodes and edges.
ugraph :: String -> [Node] -> [Edge] -> DotGraph
ugraph name nodes edges = UGraph name nodes edges

--- A node of a dot graph consists of a name and a list of attributes
--- for this node.
data Node = Node String [(String,String)]

--- An edge of a dot graph consists of the names of the source and target node
--- and a list of attributes for this edge.
data Edge = Edge String String [(String,String)]

--------------------------------------------------------------------------
--- Visualize a DOT graph with the `dotviewcommand` specified in
--- the rc file of the Curry system.
-- A dependency graph consists of a list of triples of the form (n,as,ms),
-- where n is a node name, as (dot) attributes for node n, and ms the list
-- of direct dependents from n.
viewDotGraph :: DotGraph -> IO ()
viewDotGraph = viewDot . showDotGraph

--- Shows a Dot graph as a string of the DOT language.
showDotGraph :: DotGraph -> String
showDotGraph g = showDotGraphWithAttrs "" g

--- Shows a Dot graph as a string of the DOT language.
--- The second argument contains a string of graph attributes
--- of the DOT languages, e.g., `ordering=out;'.
showDotGraphWithAttrs :: String -> DotGraph -> String
showDotGraphWithAttrs attrs (DGraph name nodes edges) =
  "digraph \"" ++ name ++ "\"" ++ graphbody2dot True attrs nodes edges
showDotGraphWithAttrs attrs (UGraph name nodes edges) =
  "graph \"" ++ name ++ "\"" ++ graphbody2dot False attrs nodes edges

graphbody2dot :: Bool -> String -> [Node] -> [Edge] -> String
graphbody2dot directed attrs nodes edges =
  "{\n" ++ (if null attrs then "" else attrs ++ "\n")
        ++ concatMap node2dot nodes
        ++ concatMap (edge2dot directed) edges ++ "}\n"

node2dot :: Node -> String
node2dot (Node nname attrs) =
  showDotID nname ++ showDotAttrs attrs ++ ";\n"

edge2dot :: Bool -> Edge -> String
edge2dot directed (Edge i j attrs) =
  showDotID i ++ edgeOp ++ showDotID j ++ showDotAttrs attrs ++ ";\n"
 where
  edgeOp = if directed then " -> " else " -- "

showDotAttrs :: [(String, String)] -> String
showDotAttrs attrs =
  if null attrs then ""
                else '[' : intercalate "," (map showDotAttr attrs) ++ "]"

--- Shows an attribute of a graph as a string of the DOT language.
--- If the attribute name is `label` and its value is enclosed in
--- angle brackets, it is shown as an HTML-like label, otherwise it is
--- enclosed in quotation marks.
showDotAttr :: (String,String) -> String
showDotAttr (name,value)
 | name == "label" && not (null value) && head value == '<' && last value == '>'
 = "label=" ++ value
 | otherwise
 = name ++ "=\"" ++ value ++ "\""

showDotID :: String -> String
showDotID s | all isAlphaNum s = s
            | otherwise        = '"' : concatMap escapeDQ s ++ "\""
 where
  escapeDQ c = if c=='"' then "\\\"" else [c]

--- Visualize a string of the DOT langugage with the `dotviewcommand`
--- from the rc file of the Curry system.
viewDot :: String -> IO ()
viewDot dottxt = do
    dotview <- getDotViewCmd
    dotstr <- connectToCommand dotview
    hPutStr dotstr dottxt
    hClose dotstr

-------------------------------------------------------------------------
--- Read the command for viewing dot files from the rc file of the
--- Curry system.
getDotViewCmd :: IO String
getDotViewCmd = do
  rcfile <- rcFileName
  getPropertyFromFile rcfile "dotviewcommand" >>= return . maybe "" id

--- Sets the command for viewing dot files in the rc file of the
--- Curry system.
setDotViewCmd :: String -> IO ()
setDotViewCmd dvcmd = do
  rcfile <- rcFileName
  updatePropertyFile rcfile "dotviewcommand" dvcmd

-------------------------------------------------------------------------
{-

Example settings in rc file:

dotviewcommand=dot -Tpdf > /tmp/dotxxx && acroread /tmp/dotxxx
dotviewcommand=neato -Tpdf > /tmp/dotxxx && acroread /tmp/dotxxx
dotviewcommand=circo -Tpdf > /tmp/dotxxx && acroread /tmp/dotxxx
dotviewcommand=fdp -Tpdf > /tmp/dotxxx && acroread /tmp/dotxxx

-}
