------------------------------------------------------------------------------
--- Definition and processing of options for the ICurry compiler.
---
--- @author Michael Hanus, Sascha Ecks
--- @version July 2022
------------------------------------------------------------------------------

module ICurry.Options
 where

import Control.Monad         ( when, unless )
import Data.List             ( union )
import Numeric               ( readNat )
import Data.Maybe            ( fromMaybe )
import System.Console.GetOpt

import qualified Data.Map as Map
import FlatCurry.Types       ( QName )
import ICurry.Types          ( IArity )
import System.CurryPath      ( currySubdir )
import System.Directory      ( getAbsolutePath )
import System.FrontendExec   ( FrontendParams, defaultParams, setQuiet )
import System.Process        ( exitWith )


------------------------------------------------------------------------------
--- Options for the ICurry compiler.
--- Contains mappings from constructor and functions names
--- into locally unique integers and other stuff.
data ICOptions = ICOptions
  { optVerb        :: Int    -- verbosity
                             -- (0: quiet, 1: status, 2: intermediate, 3: all)
  , optHelp        :: Bool   -- if help info should be printed
  , optLift        :: Bool   -- should nested cases/lets be lifted to top-level?
  , optOutput      :: String -- name of output file (or null)
  , optMain        :: String -- name of main function
  , optShowGraph   :: Int    -- level to visualize graph during execution:
                             -- 0: do not show, 1: show graph,
                             -- 2: show full graph, 3: show full with node IDs
  , optViewPDF     :: String -- command to view graph PDF
  , optInteractive :: Bool   -- interactive execution?
  , optVarDecls    :: Bool   -- optimize variable declarations?
  , optFrontendParams :: FrontendParams
  -- internal options
  -- list of module names where the constructor/functions are stored
  -- in the optConsMap and optFunMap
  , optModsMaps    :: [String]
   -- map qualified cons names to arity/position:
  , optConsMap     :: [(String, Map.Map String (IArity,Int))]
   -- map qualified function names to indices:
  , optFunMap      :: [(String, Map.Map String Int)]
  , optFun         :: QName    -- currently compiled function
  , optTermGraph   :: Bool     -- generate term graph representation
  , optXMLOutput   :: String   -- name of output file for XML term graph
  , optGraphOutput :: String   -- name of output file for SVG term graphs
  , optTreeOutput  :: String   -- name of output file for SVG tree graphs
  , optShowNodeIDs :: Bool     -- should node-labels in SVGs contain NodeIDs?
  , optTreeDepth   :: Int      -- max Depth for tree visualization
  , optMaxSteps    :: Int      -- max number of computation steps
  }

-- The default options with empty internal options.
defaultICOptions :: ICOptions
defaultICOptions =
  ICOptions 1 False True "" "" 0 "evince" False False
            (setQuiet True defaultParams) [] [] [] ("","") False "" "" "" False 10 (-1)

-- Sets the internal constructor and function maps from given lists.
setConsFuns :: ICOptions -> [(String, [(QName,(IArity,Int))])]
            -> [(String, [(QName,Int)])] -> ICOptions
setConsFuns opts modconslist modfunlist =
  opts { optConsMap = foldr addIfNotPresent (optConsMap opts) modconslist
       , optFunMap  = foldr addIfNotPresent (optFunMap  opts) modfunlist
       , optModsMaps = union (map fst modconslist)
                             (union (map fst modfunlist) (optModsMaps opts))
       }
 where
  addIfNotPresent (mn,nameinfos) infomap =
    if mn `elem` optModsMaps opts
      then infomap
      else foldr addQMap infomap nameinfos

-- Adds the info for a qualified name in a map.
addQMap :: (QName,a) -> [(String, Map.Map String a)]
        -> [(String, Map.Map String a)]
addQMap ((mn,fn),i) [] = [(mn, Map.singleton fn i)]
addQMap (qn@(mn,fn),i) ((m,mmap):mmaps) =
  if mn == m then (m, Map.insert fn i mmap) : mmaps
             else (m,mmap) : addQMap (qn,i) mmaps

-- Looks up the info for a qualified name in a map.
qmapLookup :: QName -> [(String, Map.Map String a)] -> Maybe a
qmapLookup (mn,fn) mmap =
  maybe Nothing
        (\fm -> Map.lookup fn fm)
        (lookup mn mmap)

-- Lookup arity and position index of a constructor.
arityPosOfCons :: ICOptions -> QName -> (IArity,Int)
arityPosOfCons opts qn =
  maybe (funError opts $ "Internal error in ICurry.Compiler:\n" ++
           "arity of constructor " ++ showQName qn ++ " is unknown")
        id
        (qmapLookup qn (optConsMap opts))

-- Lookup position index of a constructor.
posOfCons :: ICOptions -> QName -> Int
posOfCons opts qn = snd (arityPosOfCons opts qn)

posOfFun :: ICOptions -> QName -> Int
posOfFun opts qn =
  maybe (funError opts $ "Internal error in ICurry.Compiler:\n" ++
           "arity of operation " ++ showQName qn ++ " is unknown")
        id
        (qmapLookup qn (optFunMap opts))

printStatus :: ICOptions -> String -> IO ()
printStatus opts s = when (optVerb opts > 0) $ putStrLn s

printIntermediate :: ICOptions -> String -> IO ()
printIntermediate opts s = when (optVerb opts > 1) $ putStrLn s

printDetails :: ICOptions -> String -> IO ()
printDetails opts s = when (optVerb opts > 2) $ putStrLn s

funError :: ICOptions -> String -> _
funError opts err = error $ "Function '" ++ snd (optFun opts) ++ "': " ++ err

------------------------------------------------------------------------------
--- Process the actual command line argument and return the options
--- and the name of the main program.
processOptions :: String -> [String] -> IO (ICOptions,[String])
processOptions banner argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultICOptions funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts) (printUsage >> exitWith 0)
  when (not (null (optMain opts)) && not (optLift opts)) $ error
    "Incompatible options: interpreter requires case/let lifting!"
  let out = optOutput opts
  opts1 <- if null out || out == "-" then return opts
                                     else do aout <- getAbsolutePath out
                                             return opts { optOutput = aout }
  return (opts1, args)
 where
  printUsage = putStrLn (banner ++ "\n" ++ usageText)

-- Help text
usageText :: String
usageText = usageInfo ("Usage: icurry [options] <module name>\n") options

-- Definition of actual command line options.
options :: [OptDescr (ICOptions -> ICOptions)]
options =
  [ Option "h?" ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print help and exit"
  , Option "q" ["quiet"]
           (NoArg (\opts -> opts { optVerb = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verbosity"]
            (OptArg (maybe (checkVerb 2) (safeReadNat checkVerb)) "<n>")
            "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show generated program (same as `-v')\n3: show all details"
  , Option "o" ["output"]
           (ReqArg (\s opts -> opts { optOutput = s }) "<f>")
           ("output file for ICurry program (or '-')\n(otherwise: store in " ++
            currySubdir ++ "/MOD.icy)\nor PDF containing term graphs (with option '-g')")
  , Option "m" ["main"]
           (ReqArg (\s opts -> opts { optMain = s }) "<f>")
           "name of the main function to be interpreted\n(otherwise the ICurry program is stored)"
  , Option "g" ["graph"]
            (OptArg (maybe (checkGraph 1) (safeReadNat checkGraph)) "<n>")
            ("level to visualize term graph during execution:\n" ++
             "0: do not show term graph\n" ++
             "1: show term graph (same as `-g`)\n   (requires 'dot' and '" ++
            viewer ++ "')\n" ++
             "2: show full term graph\n3: show full graph with node IDs")
  , Option "" ["viewer"]
           (ReqArg (\s opts -> opts { optViewPDF = s }) "<c>")
           ("command to view PDF files (default: '" ++ viewer ++ "')")
  , Option "i" ["interactive"]
           (NoArg (\opts -> opts { optInteractive = True }))
           "interactive execution (ask after each step/result)"
  , Option "" ["nolifting"]
           (NoArg (\opts -> opts { optLift = False }))
           "do not lift nested case/let expressions"
  , Option "" ["optvardecls"]
           (NoArg (\opts -> opts { optVarDecls = True }))
           "do not generate variable declarations when\nvariables are introduced by assignments"
  , Option "" ["graphxml"]
           (OptArg (\s opts -> opts { optTermGraph = True
                                , optXMLOutput = (fromMaybe "icurryGraph" s) })
                   "<f>")
           "store XML representation of term graphs\nfor each computation step in file <f>.xml"
  , Option "" ["graphsvg"]
           (OptArg (\s opts -> opts { optTermGraph = True
                             , optGraphOutput = (fromMaybe "icurryGraphs" s) })
                   "<d>")
           "store SVG representations of term graphs\nfor each computation step in directory <d>"
  , Option "" ["treesvg"]
           (OptArg (\s opts -> opts { optTermGraph = True
                                , optTreeOutput = (fromMaybe "icurryTree" s) })
                   "<d>")
           "store SVG representations of term graphs as trees\nfor each computation step in directory <d>"
  , Option "" ["shownodeids"]
           (NoArg (\opts -> opts { optShowNodeIDs = True }))
           "show NodeIDs in visualized graphs"
  , Option "" ["maxdepth"]
           (ReqArg (safeReadNat checkDepth) "<n>")
           "max depth for tree visualization, default is 10"
  , Option "" ["maxsteps"]
           (ReqArg (safeReadNat checkMaxSteps) "<n>")
           "max number of computation steps, default is 100"
  ]
 where
  viewer = optViewPDF defaultICOptions

  safeReadNat opttrans s opts = case readNat s of
    [(n,"")] -> opttrans n opts
    _        -> error "Illegal number argument (try `-h' for help)"

  checkVerb n opts = if n>=0 && n<4
                       then opts { optVerb = n }
                       else error "Illegal verbosity level (try `-h' for help)"

  checkGraph n opts = if n>=0 && n<4
                        then opts { optShowGraph = n }
                        else error "Illegal graph level (try `-h' for help)"

  checkDepth n opts = if n>=0
                        then opts { optTreeDepth = n }
                        else error "Illegal max depth (try `-h' for help)"

  checkMaxSteps n opts = if n>0
                        then opts { optMaxSteps = n }
                        else error "Illegal max steps (try `-h' for help)"

------------------------------------------------------------------------------
-- Auxiliaries:

showQName :: QName -> String
showQName (mn,fn) = mn ++ "." ++ fn

------------------------------------------------------------------------------
