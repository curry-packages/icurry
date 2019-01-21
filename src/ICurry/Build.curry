--- Module for simpler generation of Ninja build files for ICurry
--- @author Marc Andre Wittorf

module ICurry.Build(
  generateFlatCurryEdges, generateFlatCurryEdge,
  generateICurryEdges, generateICurryEdge,
  generateEdges,
  tfcyRule, icyRule,
  curryToFlatPath
) where

-- generates ninja files to build icurry/extended icurry files from curry
-- source files

import List
import Maybe
import FilePath

import System.CurryPath

import ICurry.DependencyResolution
import ICurry.Files
import Ninja.Types

--- Generate build edges to translate Curry to Typed FlatCurry
--- @param deps all modules with dependencies
--- @return     (build edge) ninja declarations
generateFlatCurryEdges :: [ModuleDep] -> [Decl]
generateFlatCurryEdges deps = map (generateFlatCurryEdge deps) deps

--- Generate a build edge to translate Curry to Typed FlatCurry
--- @param allDeps all dependencies
--- @param dep     a module with its dependencies
--- @return        a (build edge) ninja declaration
generateFlatCurryEdge :: [ModuleDep] -> ModuleDep -> Decl
generateFlatCurryEdge allDeps (ModuleDep modname path deps) =
    Edge "tfcy" [curryToFlatPath modname path]
                []
                [path]
                (map transDep deps)
                []
                []
  where
    transDep d = curryToFlatPath d $
                 modulePath $
                 fromJust $
                 find ((d ==) . moduleName)
                 allDeps

--- Find the path to a Typed FlatCurry file
--- @param m    the module parts
--- @param path the path to the curry file
--- @return     the path to the tfcy file
curryToFlatPath :: String -> FilePath -> FilePath
curryToFlatPath []      p = replaceExtension p "tfcy"
curryToFlatPath m@(_:_) p = replaceExtension (inCurrySubdirModule m p) "tfcy"


--- Generate build edges to translate Typed FlatCurry to ICurry
--- @param prefix the icy files will be placed here
--- @param depd   modules with their dependencies
--- @return       (build edge) ninja declarations
generateICurryEdges :: FilePath -> [ModuleDep] -> [Decl]
generateICurryEdges prefix = map (generateICurryEdge prefix)

--- Generate a build edge to translate Typed FlatCurry to ICurry
--- @param prefix the icy file will be placed here
--- @param dep    a module with its dependencies
--- @return       a (build edge) ninja declaration
generateICurryEdge :: FilePath -> ModuleDep -> Decl
generateICurryEdge prefix (ModuleDep modname path deps) =
  Edge "icy" [prefix </> modNameToPath modname <.> "icy"]
       [prefix </> modNameToPath modname <.> "ictdeps"]
       [curryToFlatPath modname path]
       (map (\d -> prefix </> modNameToPath d <.> "ictdeps") deps)
       []
       []

--- Automatically generate edges for a dependency graph
---
---   generateEdges 'f2i' 'tfcy' 'icy' True '/path' deps
--- This generates edges for the `deps` dependency graph.
--- All files are in a module structure under /path.
--- Each file *.tfcy is translated into a *.icy file by the rule f2i.
--- As wantNeighborDep is True, Prelude.icy must be compiled before MyModule.icy
--- can be compiled, assuming that Prelude is a dependency of MyModule.
--- If wantNeighborDep was False, MyModule.tfcy could be translated to
--- MyModule.icy independently from the Prelude.
---
--- @param ruleName        the rule to use
--- @param srcExt          the source file extension
--- @param tgExt           the target file extension
--- @param wantNeighborDep are targets of dependencies required?
--- @param prefix          the path where the module structure is located
--- @param depgraph        the module dependency graph
--- @return                ninja (build edge) declarations
generateEdges :: String
              -> String
              -> String
              -> Bool
              -> Maybe FilePath
              -> FilePath
              -> [ModuleDep]
              -> [Decl]
generateEdges ruleName srcExt tgExt wantNeighborDep outPrefix prefix =
    map generateEdge
  where
    generateEdge (ModuleDep modname path deps) =
      Edge ruleName
           [maybe prefix id outPrefix </> modNameToPath modname <.> tgExt]
           []
           [prefix </> modNameToPath modname <.> srcExt]
           (if wantNeighborDep
               then (map (\d -> prefix </> modNameToPath d <.> tgExt) deps)
               else [])
           []
           []

-- TODO: autodetect curry frontend and library paths

--- Ninja rule to compile Curry to Typed FlatCurry
--- @return the rule declaration
tfcyRule :: Decl
tfcyRule = Rule "tfcy"
  [("command",
    "kics2-frontend --typed-flat \
     \-i \"$$(realpath \"$$(dirname \"$$(which kics2-frontend)\")\")/../lib\" \
     \$in")]

--- Ninja rule to compile Typed FlatCurry to ICurry
--- @return the rule declaration
icyRule :: FilePath -> Decl
icyRule path = Rule "icy"
  [("command", "icurry f2i -I \"" ++ path ++ "\" $in $out")]
