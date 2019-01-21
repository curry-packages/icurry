--- Extract 'import'-directives from Curry sources without completely parsing them
--- This is meant to be a much faster (although not completely correct) method
--- to just get this small piece of information
--- @author Marc Andre Wittorf

module ICurry.FindAllImports(
  findAllImports,
  findModuleName
) where

-- small (and probably not 100% in line with specs) module to find all imports
-- in a curry file
-- at least a line break between the `import` keyword and the module name
-- will cause problems

import Char
import List

--- Find all import directives in a module
---
--- Adds implicit Prelude
---
--- @param p the module text
--- @return  names of all imported modules
findAllImports :: String -> [String]
findAllImports p = nub $ "Prelude" : imports
  where
    ps = map sanitizeLine $ lines p
    importLines = filter isImport ps
    imports = concatMap extractImports importLines

--- Sanitize a line (ie. strip spaces)
--- @param line the unsanitized line
--- @return     the sanitized line
sanitizeLine :: String -> String
sanitizeLine = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--- Check if a line contains an import directive
--- @param line the line
--- @return     True iff the line contains a an import directive
isImport :: String -> Bool
isImport l = take 6 l == "import" && maybe False isSpace (6 `nth` l)

--- Extract all imports from an import line
--- @param l a line containing an import directive
--- @return  the names of all modules imported in this line
extractImports :: String -> [String]
extractImports l = if isImport l
                      then modname:extractImports saneRest
                      else []
  where
    trimmed = dropWhile isSpace $ drop 7 l
    isQualified = take 9 trimmed == "qualified"
                  && maybe False isSpace (9 `nth` trimmed)
    unqualified = if isQualified
                     then dropWhile isSpace $ drop 10 trimmed
                     else trimmed
    (modname, rest) = break (\c -> isSpace c
                                   || c == ';'
                                   || c == '(') unqualified
    saneRest = sanitizeLine $ dropWhile (==';') $ dropWhile (/=';') rest

--- Get the nth element of a list
--- @param n  the wanted index
--- @param xs the list
--- @return Just (xs !! n) if xs is long enough, Nothing otherwise
nth :: Int -> [a] -> Maybe a
nth _ []     = Nothing
nth n (x:xs) = case n of
                    0 -> Just x
                    _ -> nth (n-1) xs

--- Find the name of a module from its contents
--- @param p the module's contents
--- @return  the module name, if it can be determined
findModuleName :: String -> Maybe String
findModuleName p = if modulePredicate startAtModule
                      then Just moduleName
                      else Nothing
  where
    modulePredicate s = take 6 s == "module"
                        && maybe False isSpace (6 `nth` s)
    startAtModule = dropWhileRest (not . modulePredicate) p
    startAtModuleName = dropWhile isSpace $ drop 7 startAtModule
    moduleName = takeWhile (not . isSpace) startAtModuleName

--- Remove all chars from the beginning of a string, which fulfill a predicate
--- @param p  the predicate
--- @param xs the list
--- @return   the rest. The first element of this rest does fulfill p
dropWhileRest :: ([a] -> Bool) -> [a] -> [a]
dropWhileRest _ []       = []
dropWhileRest p xs@(_:xs') = if p xs
                                then dropWhileRest p xs'
                                else xs
