module Main.Flat2I where

import FilePath     ( replaceExtension )
import GetOpt
import ReadShowTerm ( readUnqualifiedTerm ) -- for faster reading
import System

import FlatCurry.Annotated.Files
import FlatCurry.Annotated.Goodies
import FlatCurry.Annotated.Types
import Text.Pretty

import ICurry.Types
import ICurry.C2I
import ICurry.Files
import ICurry.InferNeededTypeArgs
import ICurry.Pretty

main :: IO ()
main = getArgs >>= pmain

--TODO: properly handle import paths
--TODO: allow explicit specification of typedeps file?

pmain :: [String] -> IO ()
pmain args = do
  let (funopts, files, opterrors) = getOpt RequireOrder f2ioptDescrs args
      opts   = foldl (flip id) defaultF2IOptions funopts
      errors = if length files < 2
                 then "Need input file and output file" : opterrors
                 else opterrors
  if not $ null errors
    then
      mapM_ putStrLn (errors ++ [usageText])
    else do
      defpath <- defaultPaths
      let curryStyleLibdirs  = curryLibDirs opts ++ defpath
          directStyleLibdirs = libDirs opts
      tfcy <- readTypedFlatCurryFile $ files !! 0
      let imports = progImports tfcy
      typeDeps <- mapM (rtd curryStyleLibdirs directStyleLibdirs) imports
                  >>= (return . concat)
      let modNeeded    = findNeededImports typeDeps tfcy
          icurry       = flatToI (modNeeded ++ typeDeps) tfcy
          icurryPath   = files !! 1
          typeDepsPath = icurryPath `replaceExtension` "ictdeps"
      writeFile icurryPath $ show icurry
      writeFile typeDepsPath $ show modNeeded
      when (optPretty opts) $ putStrLn (pPrint (ppIProg icurry))
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
    contents <- readFile filename
    -- ...with generated Read class instances (slow!):
    -- return (read contents)
    -- ...with built-in generic read operation (faster):
    return (readUnqualifiedTerm ["ICurry.Types", "FlatCurry.Types",
                                 "Prelude"]
                                contents)

------------------------------------------------------------------------------
--- Options for the f2i compiler.
data F2IOptions = F2IOptions
  { curryLibDirs  :: [String]
  , libDirs       :: [String]
  , optPretty     :: Bool     -- show the pretty-printed ICurry program?
  }

defaultF2IOptions :: F2IOptions
defaultF2IOptions = F2IOptions [] [] False

f2ioptDescrs :: [OptDescr (F2IOptions -> F2IOptions)]
f2ioptDescrs =
 [ Option "i" []
    (ReqArg (\d opts -> opts { curryLibDirs = curryLibDirs opts ++ [d] }) "DIR")
            "Look for imported modules here, using .curry directory"
 , Option "I" []
    (ReqArg (\d opts -> opts { libDirs = libDirs opts ++ [d] }) "DIR")
            "Look for imported modules here"
 , Option "p" ["pretty"]
          (NoArg (\opts -> opts { optPretty = True }))
          "show pretty-printed ICurry program"
 ]

-- Help text
usageText :: String
usageText =
  usageInfo ("Usage: icurry f2i [options] <infile> <outfile>\n") f2ioptDescrs

------------------------------------------------------------------------------
