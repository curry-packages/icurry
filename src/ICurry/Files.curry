------------------------------------------------------------------------------
--- This library defines I/O actions to read and write ICurry programs.
---
--- @author Michael Hanus
--- @version November 2020
------------------------------------------------------------------------------

module ICurry.Files where

import ReadShowTerm        ( readUnqualifiedTerm, showTerm )
import System.CurryPath    ( inCurrySubdir, stripCurrySuffix
                           , lookupModuleSourceInLoadPath
                           )
import System.Directory    ( doesFileExist )
import System.FilePath     ( takeFileName, (</>), (<.>) )

import ICurry.Types

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding ICurry program.
iCurryFileName :: String -> String
iCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "icy"

--- Gets the standard ICurry file location for a given Curry module name
--- The Curry source program must exist in the Curry load path,
--- otherwise an error is raised.
iCurryFilePath :: String -> IO String
iCurryFilePath mname = do
  mbsrc <- lookupModuleSourceInLoadPath mname
  case mbsrc of
    Nothing      -> error $ "Curry source file for module '" ++ mname ++
                            "' not found!"
    Just (dir,_) -> return (iCurryFileName (dir </> mname))

--- Reads an ICurry program from a file in ".icy" format.
--- The argument is the name of the corresponding Curry program.
readICurry :: String -> IO IProg
readICurry progname = iCurryFilePath progname >>= readICurryFile

--- Reads an ICurry program from a file in ".icy" format
--- where the file name is provided as the argument.
readICurryFile :: String -> IO IProg
readICurryFile filename = do
  exicy <- doesFileExist filename
  if exicy
    then do contents <- readFile filename
            return (readUnqualifiedTerm ["ICurry.Types","Prelude"] contents)
    else error $ "EXISTENCE ERROR: ICurry file '" ++ filename ++
                 "' does not exist"

--- Writes an ICurry program into a file in ".icy" format.
--- The file is written in the standard location for intermediate files,
--- i.e., in the 'iCurryFileName' relative to the directory of the
--- Curry source program (which must exist!).
writeICurry :: IProg -> IO ()
writeICurry prog@(IProg mname _ _ _) = do
  fname <- iCurryFilePath mname
  writeICurryFile fname prog

--- Writes an ICurry program into a file in ".icy" format.
--- The first argument must be the name of the target file
--- (with suffix ".icy").
writeICurryFile :: String -> IProg -> IO ()
writeICurryFile file prog = writeFile file (showTerm prog)
