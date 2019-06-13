--- Module for handling ICurry and related files
--- @author Marc Andre Wittorf

module ICurry.Files where

import Directory     ( doesFileExist )
import ReadShowTerm  ( readUnqualifiedTerm ) -- for faster reading
import System

import ICurry.Types
import FlatCurry.Types
import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Files
import FlatCurry.Annotated.TypeInference
import FileGoodies hiding (splitPath)
import FilePath
import Distribution

import System.CurryPath
import System.FrontendExec


--- Default search paths (obtained from CURRYPATH environment variable)
defaultPaths :: IO [String]
defaultPaths = do
  currypath <- getEnviron "CURRYPATH"
  return $ if null currypath
              then []
              else splitSearchPath currypath ++ sysLibPath

--- Basically the Maybe functor instance
--- @param f the function to transform a Just constructor's content
--- @param v the value
--- @return  the transformed value
mapJust :: (a -> b) -> Maybe a -> Maybe b
mapJust _ Nothing  = Nothing
mapJust f (Just x) = Just $ f x

--- Bindable IO action to normalize a file path
--- @param fp the file path
--- @return   the normalized file path (if applicable)
normaliser :: Maybe FilePath -> IO (Maybe FilePath)
normaliser = return . mapJust normalise

--- Make a lookup (returns Maybe) function to a get function (uses error)
--- @param lookupper the lookup function
--- @param err       the fallback function if nothing is found
--- @param paths     the lookup paths
--- @param modname   the module name to search
--- @return          the lookup function's result (or err if not found)
lookupToGet :: ([String] -> String -> IO (Maybe String))
            -> IO String
            -> [String]
            -> String
            -> IO String
lookupToGet lookupper err paths modname = do
  res <- lookupper paths modname
  case res of
       Nothing -> err
       Just x  -> return x

--- Look up a Curry file in the search paths
--- @param paths   the search paths
--- @param modname the module name
--- @return        the module's path
lookupCurryFile :: [String] -> String -> IO (Maybe String)
lookupCurryFile paths modname = lookupFileInPath base
                                                 [".curry", ".lcurry"]
                                                 (map (</> dir) paths)
                                >>= normaliser
  where
    (dir, base) = splitDirectoryBaseName $ modNameToPath modname

--- Get a Curry file in the search paths. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the module's path
getCurryFile :: [String] -> String -> IO String
getCurryFile = lookupToGet lookupCurryFile (error "Cannot find Curry file")

--- Look up a Typed FlatCurry file in the search paths
--- @param paths   the search paths
--- @param modname the module name
--- @return        the FlatCurry module's path
lookupFlatFile :: [String] -> String -> IO (Maybe String)
lookupFlatFile paths modname =
    lookupFileInPath base
                     [workaround ".fcy" ".tfcy"]
                     (map ((</> dir) . addCurrySubdir) paths)
    >>= normaliser
  where
    (dir, base) = splitDirectoryBaseName $ modNameToPath modname

--- Get a Typed FlatCurry file in the search paths. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the FlatCurry module's path
getFlatFile :: [String] -> String -> IO String
getFlatFile = lookupToGet lookupFlatFile
                          (error "Cannot find Typed FlatCurry file")

--- Look up a Type Dependency file in the search paths
--- @param paths   the search paths
--- @param modname the module name
--- @return        the Type Dependecy file's path
lookupTypeDepsFile :: [String] -> String -> IO (Maybe String)
lookupTypeDepsFile paths modname =
    lookupFileInPath base
                     [".ictdeps"]
                     (map ((</> dir) . addCurrySubdir) paths)
    >>= normaliser
  where
    (dir, base) = splitDirectoryBaseName $ modNameToPath modname

--- Look up a Type Dependency file. Don't append the .curry subdirectory
--- @param paths   the search paths
--- @param modname the module name
--- @return        the Type Dependecy file's path
lookupTypeDepsFileRaw :: [String] -> String -> IO (Maybe String)
lookupTypeDepsFileRaw paths modname =
    lookupFileInPath base
                     [".ictdeps"]
                     (map (</> dir) paths)
    >>= normaliser
  where
    (dir, base) = splitDirectoryBaseName $ modNameToPath modname

--- Get a Type Dependency file in the search paths. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the Type Dependecy file's path
getTypeDepsFile :: [String] -> String -> IO String
getTypeDepsFile =
  lookupToGet lookupTypeDepsFile
              (error "Cannot find ICurry type dependencies file")

--- Get a Type Dependency file. Don't append the .curry subdirectory. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the Type Dependecy file's path
getTypeDepsFileRaw :: [String] -> String -> IO String
getTypeDepsFileRaw =
  lookupToGet lookupTypeDepsFileRaw
              (error "Cannot find ICurry type dependencies file")

--- Look up an ICurry file in the search paths
--- @param paths   the search paths
--- @param modname the module name
--- @return        the ICurry module's path
lookupICurryFile :: [String] -> String -> IO (Maybe String)
lookupICurryFile paths modname =
    lookupFileInPath base
                     [".icy"]
                     (map ((</> dir) . addCurrySubdir) paths)
    >>= normaliser
  where
    (dir, base) = splitDirectoryBaseName $ modNameToPath modname

--- Look up an ICurry file in the search paths. Don't append .curry subdirectory
--- @param paths   the search paths
--- @param modname the module name
--- @return        the ICurry module's path
lookupICurryFileRaw :: [String] -> String -> IO (Maybe String)
lookupICurryFileRaw paths modname = lookupFileInPath base
                                                     [".icy"]
                                                     (map (</> dir) paths)
                                    >>= normaliser
  where
    (dir, base) = splitDirectoryBaseName $ modNameToPath modname

--- Get an ICurry file in the search paths. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the ICurry module's path
getICurryFile :: [String] -> String -> IO String
getICurryFile = lookupToGet lookupICurryFile
                            (error "Cannot find ICurry file")

--- Get an ICurry file in the search paths. Don't append .curry subdirectory. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the ICurry module's path
getICurryFileRaw :: [String] -> String -> IO String
getICurryFileRaw = lookupToGet lookupICurryFileRaw
                               (error "Cannot find ICurry file")

--- Get the root directory where a module is located under
---
--- Example: a module Foo.Bar that can be found under /dir/subdir/Foo/Bar.curry
--- will cause a return value of /dir/subdir
---
--- @param paths   the search paths
--- @param modname the module name
--- @return        the source's root directory
getPathForModule :: [String] ->  String -> IO String
getPathForModule paths modname = do
  curryfile <- getCurryFile paths modname
  let s = splitDirectories curryfile
      modIds = splitModuleIdentifiers modname
      l = length $ modIds
      pathParts = take (length s - l) s
      path = joinPath $ pathParts ++ currySubdir : modIds
  return path

--- Read a Type Dependency file. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the Type Dependencies
readTypeDeps :: [String] -> String -> IO [NeededMapping]
readTypeDeps paths modname = do
  fname <- getTypeDepsFile paths modname
  contents <- readFile fname
  -- ...with generated Read class instances (slow!):
  -- return $ read contents
  -- ...with built-in generic read operation (faster):
  return (readUnqualifiedTerm ["ICurry.Types", "FlatCurry.Types",
                               "Prelude"]
                              contents)

--- Read an ICurry file. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the ICurry abstract representation
readICurry :: [String] -> String -> IO IProg
readICurry paths modname =
  getICurryFile paths modname >>= readICurryFile

--- Read an ICurry file. Don't append .curry subdir. Error if not found
--- @param paths   the search paths
--- @param modname the module name
--- @return        the ICurry abstract representation
readICurryRaw :: [String] -> String -> IO IProg
readICurryRaw paths modname =
  getICurryFileRaw paths modname >>= readICurryFile

--- Reads a file containing an ICurry term.
--- @param filename   the file name
--- @return           the ICurry abstract representation
readICurryFile :: String -> IO IProg
readICurryFile filename = do
  exfile <- doesFileExist filename
  if exfile
   then do contents <- readFile filename
           -- ...with generated Read class instances (slow!):
           -- return (read contents)
           -- ...with built-in generic read operation (faster):
           return (readUnqualifiedTerm ["ICurry.Types", "FlatCurry.Types",
                                        "Prelude"]
                                       contents)
   else error $ "EXISTENCE ERROR: ICurry file '" ++ filename ++
                "' does not exist"

--- Write a Type Dependency file. Find target directory based on source file
--- @param paths   the search paths
--- @param modname the module name (for finding correct path)
--- @param ms      the Type Dependencies
writeTypeDeps :: [String] -> String -> [NeededMapping] -> IO ()
writeTypeDeps paths modname ms = do
  filename <- getPathForModule paths modname >>= return . (<.> "ictdeps")
  writeFile filename $ show ms

--- Write an ICurry file. Find target directory based on source file
--- @param paths   the search paths
--- @param modname the module name (for finding correct path)
--- @param prog      the ICurry module
writeICurry :: [String] -> String -> IProg -> IO ()
writeICurry paths modname prog = do
  filename <- getPathForModule paths modname >>= return . (<.> "icy")
  writeFile filename $ show prog

--- The correct target for frontend invocation to translate to Typed FlatCurry
icurryFrontendTarget :: FrontendTarget
icurryFrontendTarget = workaround FCY TFCY

--- Get the module root path from a module path and its name
--- @param modname the module name
--- @param path    the module's path
--- @return        the root path
moduleRoot :: String -> FilePath -> FilePath
moduleRoot modname path = concat $ take (partsLen - remove) pathParts
  where
    remove = length $ splitModuleIdentifiers modname
    pathParts = splitPath path
    partsLen = length pathParts

--- Dispatch something based on if a compiler version is buggy
--- @param yes use this if buggy
--- @param no  use this if not buggy
--- @return    yes or no
workaround :: a -> a -> a
workaround yes no = if curryCompiler == "pakcs"
                       && (curryCompilerMajorVersion,
                           curryCompilerMinorVersion) <= (2,0)
                       then yes
                       else no
