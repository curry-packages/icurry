--- Some convenience functions for dealing with Extended ICurry structures
--- @author Marc Andre Wittorf

module ICurry.Extended.Goodies(
  isExternal, isGlobal,
  isPublic, isPrivate,
  escapeSpecials
) where

import ICurry.Extended.Types

--- Check if a function is defined externally
--- @param f the function
--- @return  True iff the function is defined externally
isExternal :: IEFunction -> Bool
isExternal f = case f of
                    IEFunction _ _ (IEExternal _ _) -> True
                    _                               -> False

--- Check if a function is a call to the special 'global' function
--- @param f the function
--- @return  True iff the function is a global
isGlobal :: IEFunction -> Bool
isGlobal f = case f of
  IEFunction _ _ (IEFuncBody [] (IESimpleBlock _ (IEFCall ((mn,ln),_) es))) ->
      mn == "Global" && ln == "global" && length es == 2
  _ -> False

--- Escape special characters in a string
---
--- This includes many special characters common in curry which are no valid
--- identifiers in many imperative languages
---
--- @param s the unescaped string
--- @param   the escaped string
escapeSpecials :: String -> String
escapeSpecials = concatMap replaceBadChars
  where
    replaceBadChars c = case c of -- q w still available
      '_'       -> "_u" --Underscore
      ','       -> "_c" --Comma
      '.'       -> "_d" --Dot
      ':'       -> "_n" --coloN
      '#'       -> "_h" --Hash
      '+'       -> "_p" --Plus
      '-'       -> "_m" --Minus
      '*'       -> "_y" --multiplY
      '/'       -> "_v" --diV
      '\''      -> "_t" --Tick
      '<'       -> "_l" --Lower
      '>'       -> "_g" --Greater
      '='       -> "_e" --Equal
      '('       -> "_o" --parens Open
      ')'       -> "_s" --parens cloSe
      '['       -> "_b" --Brackets open
      ']'       -> "_k" --bracKets close
      '&'       -> "_a" --Ampersand
      '|'       -> "_i" --pIpe
      '$'       -> "_r" --dollaR sign
      '!'       -> "_x" --eXclamation mark
      '?'       -> "_j" --questJon mark (running out of letters here)
      '\\'      -> "_z" --backZlash
      '^'       -> "_f" --circumFlex
      otherwise -> [c]

--- Predicate to test if a visibility is Public
--- @param v the visibility
--- @return  True if v is Public, False otherwise
isPublic :: IEVisibility -> Bool
isPublic Public  = True
isPublic Private = False

--- Predicate to test if a visibility is Private
--- @param v the visibility
--- @return  True if v is Private, False otherwise
isPrivate :: IEVisibility -> Bool
isPrivate Private = True
isPrivate Public  = False
