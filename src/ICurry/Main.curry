------------------------------------------------------------------------------
--- This module contains a simple compiler from FlatCurry to ICurry programs.
---
--- @author Michael Hanus
--- @version January 2021
------------------------------------------------------------------------------

module ICurry.Main where

import Control.Monad         ( when, unless )
import Numeric               ( readNat )
import System.Environment    ( getArgs )
import System.Console.GetOpt

import System.CurryPath      ( runModuleAction )
import System.Path           ( fileInPath )
import System.Process        ( exitWith )

import ICurry.Compiler
import ICurry.Files
import ICurry.Interpreter
import ICurry.Types

test :: String -> IO ()
test = icurryOnModule defaultICOptions { optVerb = 3, optMain = "main" }

testI :: String -> IO ()
testI =
  icurryOnModule defaultICOptions { optVerb = 3, optMain = "main"
                                  , optShowGraph = True, optInteractive = True }

------------------------------------------------------------------------------
banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "ICurry Compiler (Version of 11/01/21)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  args <- getArgs
  (opts,progs) <- processOptions args
  case progs of
    []  -> error "Module name missing"
    [p] -> runModuleAction (icurryOnModule opts) p
    _   -> error "Too many module names provided"

checkExecutables :: ICOptions -> IO ()
checkExecutables opts =
  if null (optMain opts) || not (optShowGraph opts)
    then return ()
    else mapM_ checkExec ["dot", optViewPDF opts]
 where
  checkExec p = do
    exp <- fileInPath p
    unless exp $ do
      putStrLn $ "Executable '" ++ p ++ "' not found in path: " ++
                 "icurry interpreter terminated"
      exitWith 1

icurryOnModule :: ICOptions -> String -> IO ()
icurryOnModule opts modname = do
  checkExecutables opts
  iprog <- icCompile opts modname
  let imain = optMain opts
  if null imain
    then do
      icyname <- iCurryFilePath modname
      writeICurryFile icyname iprog
      printStatus opts $ "ICurry program written to '" ++ icyname ++ "'"
    else do
      printStatus opts $ "Executing main function '" ++ imain ++ "'..."
      let opts1 = if optShowGraph opts
                    then defOpts { withGraph = True, waitTime = 1
                                 , withViewer = optViewPDF opts }
                    else defOpts
          opts2 = if optInteractive opts
                    then if optShowGraph opts
                           then opts1 { interactive = True, verbosity = 2 }
                           else opts1 { interactive = True }
                    else opts1
      execIProg opts2 iprog imain

--- Process the actual command line argument and return the options
--- and the name of the main program.
processOptions :: [String] -> IO (ICOptions,[String])
processOptions argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldl (flip id) defaultICOptions funopts
  unless (null opterrors)
         (putStr (unlines opterrors) >> printUsage >> exitWith 1)
  when (optHelp opts) (printUsage >> exitWith 0)
  when (not (null (optMain opts)) && not (optLift opts)) $ error
    "Incompatible options: interpreter requires case/let lifting!"
  return (opts, args)
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
  , Option "m" ["main"]
           (ReqArg (\s opts -> opts { optMain = s }) "<f>")
           "name of the main function to be interpreted\n(otherwise the ICurry program is stored)"
  , Option "g" ["graph"]
           (NoArg (\opts -> opts { optShowGraph = True }))
           "show the term graph during execution\n(requires 'dot' and 'evince')"
  , Option "" ["viewer"]
           (ReqArg (\s opts -> opts { optViewPDF = s }) "<c>")
           "command to view PDF files (default: 'evince')"
  , Option "i" ["interactive"]
           (NoArg (\opts -> opts { optInteractive = True }))
           "interactive execution (ask after each step/result)"
  , Option "" ["nolifting"]
           (NoArg (\opts -> opts { optLift = False }))
           "do not lift nested case/let expressions"
  , Option "" ["optvardecls"]
           (NoArg (\opts -> opts { optVarDecls = True }))
           "do not generate variable declarations when\nvariables are introduced by assignments"
  ]
 where
  safeReadNat opttrans s opts = case readNat s of
    [(n,"")] -> opttrans n opts
    _        -> error "Illegal number argument (try `-h' for help)"

  checkVerb n opts = if n>=0 && n<4
                     then opts { optVerb = n }
                     else error "Illegal verbosity level (try `-h' for help)"

------------------------------------------------------------------------------
