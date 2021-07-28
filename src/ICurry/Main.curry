------------------------------------------------------------------------------
--- This module contains a simple compiler from FlatCurry to ICurry programs.
---
--- @author Michael Hanus
--- @version July 2021
------------------------------------------------------------------------------

module ICurry.Main where

import Control.Monad         ( when, unless )
import System.Environment    ( getArgs )
import System.Console.GetOpt

import ReadShowTerm          ( showTerm )
import System.CurryPath      ( runModuleAction )
import System.Path           ( fileInPath )
import System.Process        ( exitWith )

import ICurry.Compiler
import ICurry.Files
import ICurry.Interpreter
import ICurry.Options
import ICurry.Types

------------------------------------------------------------------------------
banner :: String
banner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "ICurry Compiler (Version of 28/07/21)"
  bannerLine = take (length bannerText) (repeat '=')

main :: IO ()
main = do
  args <- getArgs
  (opts,progs) <- processOptions banner args
  case progs of
    []  -> error "Module name missing"
    [p] -> runModuleAction (icurryOnModule opts) p
    _   -> error "Too many module names provided"

checkExecutables :: ICOptions -> IO ()
checkExecutables opts = do
  let execs = if null (optMain opts) || optShowGraph opts == 0
                then []
                else ["dot", optViewPDF opts] ++
                     (if null (optOutput opts) then [] else ["pdftk"])
  mapM_ checkExec execs
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
    then
      if optOutput opts == "-"
        then putStrLn (showTerm iprog)
        else do
          icyname <- if null (optOutput opts) then iCurryFilePath modname
                                              else return $ optOutput opts
          writeICurryFile icyname iprog
          printStatus opts $ "ICurry program written to '" ++ icyname ++ "'"
    else do
      printStatus opts $ "Executing main function '" ++ imain ++ "'..."
      let iopts1 = defOpts { icOptions = opts }
          iopts2 = if optShowGraph opts > 0 then iopts1 { waitTime = 1 }
                                            else iopts1
      execIProg iopts2 iprog imain

------------------------------------------------------------------------------
