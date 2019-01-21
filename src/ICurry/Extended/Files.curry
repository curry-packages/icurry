--- Module for handling Extended ICurry files
--- @author Marc Andre Wittorf

module ICurry.Extended.Files where

import FileGoodies
import FilePath

import System.CurryPath

import ICurry.Extended.Types
import ICurry.Files

--- Look up an Extended ICurry file in the search paths
--- @param paths   the search paths
--- @param modname the module name
--- @return        the module's path
lookupIECurryFile :: [String] -> String -> IO (Maybe String)
lookupIECurryFile paths modname =
    lookupFileInPath base
                     [".iecy"]
                     (map ((</> dir) . addCurrySubdir) paths)
    >>= normaliser
  where
    (dir, base) = splitDirectoryBaseName $ modNameToPath modname

--- Get an Extended ICurry file in the search paths. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the module's path
getIECurryFile :: [String] -> String -> IO String
getIECurryFile = lookupToGet lookupIECurryFile
                             (error "Cannot find IECurry file")

--- Read an Extended ICurry file. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the Extended ICurry abstract representation
readIECurry :: [String] -> String -> IO (IEProg)
readIECurry paths modname = do
  fname <- getIECurryFile paths modname
  contents <- readFile fname
  return $ read contents

--- Write an Extended ICurry file. Find target directory based on source file
--- @param paths   the search paths
--- @param modname the module name (for finding correct path)
--- @param prog    the Extended ICurry module
writeIECurry :: [String] -> String -> IEProg -> IO ()
writeIECurry paths modname prog = do
  filename <- getPathForModule paths modname >>= return . (<.> "iecy")
  writeFile filename $ show prog
