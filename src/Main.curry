module Main where

import FilePath (takeFileName)
import System

import qualified Main.Flat2I as F2I (pmain)
import qualified Main.I2E    as I2E (pmain)

import Main.ConfigPackage (packageExecutable)

--entry point
main :: IO ()
main = do
  args' <- getArgs
  case args' of
       prog:args -> maybe (\_ -> usage) id (lookup prog progs) args
       []        -> usage

type MainProg = [String] -> IO ()
progs :: [(String, MainProg)]
progs = [
  ("f2i", F2I.pmain),
  ("i2e", I2E.pmain)]

usage :: IO ()
usage = do
  let pn = takeFileName packageExecutable
  putStrLn $ unlines
    [ "Usage:"
    , ""
    , "Compile FlatCurry to ICurry:"
    , pn ++ " f2i [-i includeDir] [-I includeDir] infile outfile"
    , "  -i includeDir  specify a directory to look for ictdeps files in a"
    , "                 .curry subdirectory"
    , "  -I includeDir  specify a directory to look for ictdeps files in this"
    , "                 subdirectory"
    , "  -p             show pretty-printed ICurry program"
    , "  infile         the Typed FlatCurry input file"
    , "  outfile        the ICurry output file"
    , ""
    , "Compile ICurry to Extended ICurry:"
    , pn ++ " i2e [-I includeDir] infile outfile"
    , "  -I includeDir  specify a directory to look for included modules"
    , "  infile         the ICurry input file"
    , "  outfile        the Extended ICurry output file"]
