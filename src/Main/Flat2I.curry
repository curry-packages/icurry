module Main.Flat2I where

import System
import GetOpt
import FilePath (replaceExtension)

import FlatCurry.Annotated.Files
import FlatCurry.Annotated.Goodies
import FlatCurry.Annotated.Types

import ICurry.Types
import ICurry.C2I
import ICurry.Files
import ICurry.InferNeededTypeArgs

main :: IO ()
main = getArgs >>= pmain

--TODO: properly handle import paths
--TODO: allow explicit specification of typedeps file?

pmain :: [String] -> IO ()
pmain args = do
    let (libdirs', files, errors') = getOpt RequireOrder optDescrs args
    let errors = if length files < 2
                    then "Need inputfile and outputfile" : errors'
                    else errors'
    if not $ null errors
      then
        mapM_ putStrLn errors
      else do
        let curryStyleLibdirs = (map fst $ filter snd libdirs') ++ defaultPaths
        let directStyleLibdirs = (map fst $ filter (not . snd) libdirs')
        fileContents <- readFile $ files !! 0
        let tfcy = read fileContents
        let imports = progImports tfcy
        typeDeps <- mapM (rtd curryStyleLibdirs directStyleLibdirs) imports
                    >>= (return . concat)
        let modNeeded = findNeededImports typeDeps tfcy
        let icurry = flatToI (modNeeded ++ typeDeps) tfcy
        let icurryPath = files !! 1
        let typeDepsPath = icurryPath `replaceExtension` "ictdeps"
        writeFile icurryPath $ show icurry
        writeFile typeDepsPath $ show modNeeded
  where
    rtd :: [String] -> [String] -> String -> IO [NeededMapping]
    rtd curryStylePaths rawPaths modname = do
      rawRes <- lookupTypeDepsFileRaw rawPaths modname
      filename <- maybe (do
          cres <- lookupTypeDepsFile curryStylePaths modname
          maybe (error $ "Cannot find ICurry type dependencies file of " ++
                         modname)
                return
                cres)
        return rawRes
      readFile filename >>= return . read


optDescrs :: [OptDescr (String, Bool)]
optDescrs = [
  Option "i" [] (ReqArg (\x-> (x,True)) "DIR")
                "Look for imported modules here, using .curry directory",
  Option "I" [] (ReqArg (\x-> (x,False)) "DIR")
                "Look for imported modules here"]
