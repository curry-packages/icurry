--- Find all dependencies of modules recursively
--- @author Marc Andre Wittorf

module ICurry.DependencyResolution(
  ModuleDep(..),
  buildDepGraph
) where

import FilePath
import Either
import List

import ICurry.Files
import ICurry.FindAllImports

--- A module with its dependencies
data ModuleDep = ModuleDep
  { moduleName :: String    -- module name
  , modulePath :: FilePath  -- module source file path
  , moduleDeps :: [String]} -- names of modules that this module depends on
    deriving (Show)

--- Load a module's dependencies from a file
--- @param fname the module's file name
--- @return      a moduleDep for this module
loadModuleFileDeps :: FilePath -> IO ModuleDep
loadModuleFileDeps fname = do
  fileContents <- readFile fname
  let imports = findAllImports fileContents
  let moduleName = maybe "" id $ findModuleName fileContents
  return $ ModuleDep moduleName fname imports

--- Load module's dependencies from a file. Search the module in search paths
--- @param paths   search paths for modules
--- @param modname the module's name
--- @return        a moduleDep for this module
loadModuleDeps :: [FilePath] -> String -> IO ModuleDep
loadModuleDeps paths modname = do
  filename <- lookupCurryFile paths modname
  if modname == "Prelude"
     then return $
          ModuleDep modname
                    (maybe (error "Prelude not found. Something is fishy.")
                           id
                           filename)
                    []
     else do
    mdep <- loadModuleFileDeps $ maybe (error "Module not found") id filename
    return $ mdep{moduleName = modname}

--- Recursively load module's dependencies
--- @param paths    search paths for modules
--- @param initials the modules to find dependencies of
--- @return         moduleDeps for the modules and all dependencies
buildDepGraph :: [FilePath] -> [(Either FilePath String)] -> IO [ModuleDep]
buildDepGraph paths initials = do
    modDeps <- mapM (loadModuleDeps paths) modInitials
    fileDeps <- mapM loadModuleFileDeps fileInitials
    let deps = nubBy samePath $ modDeps ++ fileDeps
    res <- bdg deps $ nub $ concatMap moduleDeps deps
    return $ nubBy samePath res
  where
    fileInitials = nub $ map fromLeft $ filter isLeft initials
    modInitials = nub $ map fromRight $ filter isRight initials
    bdg :: [ModuleDep] -> [String] -> IO [ModuleDep]
    bdg ds [] = return ds
    bdg ds (m:ms) = if any (isMod m) ds then bdg ds ms else do
      nd <- loadModuleDeps paths m
      bdg (nd:ds) (ms ++ filter (not . (`elem` ms)) (moduleDeps nd))

--- Do moduleDeps reference modules with the same paths?
--- @param a one moduleDep
--- @param b another moduleDep
--- @return  True iff the paths are equal
samePath :: ModuleDep -> ModuleDep -> Bool
samePath a b = modulePath a == modulePath b

--- Do moduleDeps reference modules with the same name?
--- @param a one moduleDep
--- @param b another moduleDep
--- @return  True iff the module names are equal
sameModname :: ModuleDep -> ModuleDep -> Bool
sameModname a b = moduleName a == moduleName b

--- Check if a moduleDep references a module with a given name
--- @param m a module name
--- @param d another moduleDep
--- @return  True d references a module with name n
isMod :: String -> ModuleDep -> Bool
isMod m d = m == moduleName d
