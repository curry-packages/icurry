module Main.I2E where

import System
import GetOpt

import ICurry.Types
import ICurry.Files
import ICurry.Extended.I2E

main :: IO ()
main = getArgs >>= pmain

pmain :: [String] -> IO ()
pmain args = do
  let (libdirs', files, errors') = getOpt RequireOrder optDescrs args
  let errors = if length files < 2
                  then "Need in- and outfile":errors'
                  else errors'
  if not $ null errors
     then mapM_ putStrLn errors
     else do
      let infile  = files !! 0
      let outfile = files !! 1
      fileContents <- readFile infile
      let icy@(IProg modname deps _ _) = read fileContents
      let libdirs = libdirs' ++ [moduleRoot modname infile]
      depInfo <- mapM ((liftIO extractDepInfoFromI) .
                      (readICurryRaw libdirs))
                      deps
      writeFile outfile $ show $ i2eProg depInfo icy

optDescrs :: [OptDescr String]
optDescrs = [
  Option "I" [] (ReqArg id "DIR") "Look for imported modules here"]
