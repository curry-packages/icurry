------------------------------------------------------------------------------
--- This library defines I/O actions to read and write ICurry programs.
---
--- @author Michael Hanus
--- @version January 2020
------------------------------------------------------------------------------

module ICurry.Files where

import Directory       ( doesFileExist )
import FilePath        ( takeFileName, (</>), (<.>) )
import ReadShowTerm    ( readUnqualifiedTerm, showTerm )

import System.CurryPath    ( inCurrySubdir, stripCurrySuffix
                           , lookupModuleSourceInLoadPath
                           )

import ICurry.Types

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding ICurry program.
iCurryFileName :: String -> String
iCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "icy"

--- Reads an ICurry program from a file in ".icy" format.
--- The argument is the name of the corresponding Curry program.
readICurry :: String -> IO IProg
readICurry progname = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> error $ "ICurry file for program '" ++ progname ++ "' not found!"
    Just (dir,_) ->
      readICurryFile (iCurryFileName (dir </> takeFileName progname))

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

--- Writes a ICurry program into a file in ".icy" format.
--- The first argument must be the name of the target file
--- (with suffix ".icy").
writeICurryFile :: String -> IProg -> IO ()
writeICurryFile file prog = writeFile file (showTerm prog)
