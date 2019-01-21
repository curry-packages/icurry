--- Module for simpler generation of Ninja build files for Extended ICurry
--- @author Marc Andre Wittorf

module ICurry.Extended.Build(
  generateExtendedICurryEdges,
  eicyRule,
  cToEicy
) where

import FilePath
import Ninja.Types
import ICurry.DependencyResolution
import ICurry.Build

--- Generate build edges to translate ICurry to Extended ICurry
--- @param prefix the icy and eicy files are here
--- @param depd   modules with their dependencies
--- @return       (build edge) ninja declarations
generateExtendedICurryEdges :: FilePath -> [ModuleDep] -> [Decl]
generateExtendedICurryEdges = generateEdges "eicy" "icy" "eicy" True Nothing

--- Ninja rule to compile ICurry to Extended ICurry
--- @return the rule declaration
eicyRule :: FilePath -> Decl
eicyRule path = Rule "eicy"
  [("command", "icurry i2e -I \"" ++ path ++ "\" $in $out")]

--- Rules and Edges to compile Curry modules to Extended ICurry
--- @param buildDir the path where I- and Extended ICurry files will be stored
--- @param dg       all modules with their dependencies
--- @return         ninja declarations
cToEicy :: String -> [ModuleDep] -> [Decl]
cToEicy buildDir dg = [tfcyRule, icyRule buildDir, eicyRule buildDir]
                      ++ generateFlatCurryEdges dg
                      ++ generateICurryEdges buildDir dg
                      ++ generateExtendedICurryEdges buildDir dg
