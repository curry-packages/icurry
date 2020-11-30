------------------------------------------------------------------------------
--- This module contains a pretty printer for ICurry programs.
---
--- @author Marc Andre Wittorf and Michael Hanus
--- @version November 2020
------------------------------------------------------------------------------

module ICurry.Pretty where

import Prelude hiding ( empty )

import Data.List ( intercalate, intersperse )

import ICurry.Types
import Text.Pretty

--- Pretty print an ICurry module
--- @param prog the module
--- @return     the pretty printed module
ppIProg :: IProg -> Doc
ppIProg (IProg name imports types funcs) = vsepBlank
  [ ppHeader name
  , ppImports imports
  , ppDataTypes types
  , ppFunctions funcs
  ]

--- Pretty print an ICurry module header
--- @param name the module name
--- @return     the pretty printed module header
ppHeader :: String -> Doc
ppHeader name = text "module" <+> text name <+> text "where"

--- Pretty print an ICurry module import list
--- @param names the imported modules' names
--- @return      the pretty printed import list
ppImports :: [String] -> Doc
ppImports = vsep . map ppImport

--- Pretty print an import directive
--- @param name the imported module's name
--- @return     the pretty printed module import directive
ppImport :: String -> Doc
ppImport name = text "import" <+> text name

--- Pretty print ICurry types
--- @param dts the data type declarations
--- @return    the pretty printed types
ppDataTypes :: [IDataType] -> Doc
ppDataTypes = vsepBlank . map ppDataType

--- Pretty print an ICurry type
--- @param dt the data type declaration
--- @return   the pretty printed type
ppDataType :: IDataType -> Doc
ppDataType (IDataType name cs) = nest 1 $
  text "data" <+> ppQName name <+> equals <+> ppConstructors cs

--- Pretty print ICurry constructors
--- @param cs the data constructors
--- @return   the pretty printed constructors
ppConstructors :: [(IQName,IArity)] -> Doc
ppConstructors cs = hsep $ intersperse bar (map ppConstructor cs)

--- Pretty print an ICurry constructor
--- @param c the data constructor
--- @return  the pretty printed constructor
ppConstructor :: (IQName,IArity) -> Doc
ppConstructor (name,ar) = ppQName name <+> char '/' <+> int ar

--- Pretty print ICurry functions
--- @param fns the functions
--- @return    the pretty printed functions
ppFunctions :: [IFunction] -> Doc
ppFunctions = vsepBlank . map ppFunction

--- Pretty print an ICurry function
--- @param fns the function
--- @return    the pretty printed function
ppFunction :: IFunction -> Doc
ppFunction (IFunction name ar _ demargs body) =
  ppQName name <> char '/' <> int ar <>
  (if null demargs
     then empty
     else text (" (DEMANDED: " ++ intercalate "," (map show demargs) ++ ")"))
  <+> char ':' <+> ppFuncBody body

--- Pretty print a qualified ICurry name (module.localname)
--- @param name the name
--- @return     the pretty printed name
ppQName :: IQName -> Doc
ppQName (modname, localname, _) = text $ modname ++ '.' : localname

--- Pretty print an ICurry function's body
--- @param body the function's body
--- @return     the pretty printed function body
ppFuncBody :: IFuncBody -> Doc
ppFuncBody (IExternal name)  = text ("external \"" ++ name ++ "\",")
ppFuncBody (IFuncBody block) = ppBlock block

--- Pretty print a list of variables
--- @param vs the variables
--- @return   the pretty printed variables
ppVars :: [IVarIndex] -> Doc
ppVars = hsep . map ppVar

--- Pretty print a variable. Variables are called x0, x1, ...
--- Since variable with index 0 is always used for the root,
--- we print it as `ROOT`.
--- @param v the variable
--- @return  the pretty printed variable
ppVar :: IVarIndex -> Doc
ppVar v | v == 0    = text "ROOT"
        | otherwise = text . ('x':) . show $ v

--- Pretty print an ICurry block
--- @param block the block
--- @return      the rendered block
ppBlock :: IBlock -> Doc
ppBlock (IBlock decls asgns stmt) = nest 2 (
  lbrace $$
  ppVarDecls decls $$
  ppAssignments asgns $$
  ppStatement stmt $$
  rbrace )

--- Pretty print an ICurry block
--- @param block the block
--- @return      the rendered block
ppStatement :: IStatement -> Doc
ppStatement IExempt          = text "exempt"
ppStatement (IReturn e)      = text "return" <+> ppExpr e
ppStatement (ICaseCons v bs) = nest 2 (
  text "case" <+> ppVar v <+> text "of" $$ ppConsBranches bs)
ppStatement (ICaseLit v bs)  = nest 2 (
  text "case" <+> ppVar v <+> text "of" $$ ppLitBranches bs)

--- Pretty print local variable declarations
--- @param vs the local variables
--- @return   the pretty printed variable declarations
ppVarDecls :: [IVarDecl] -> Doc
ppVarDecls = vcat . map ppVarDecl

--- Pretty print a local variable declaration.
ppVarDecl :: IVarDecl -> Doc
ppVarDecl (IVarDecl  v) = text "declare" <+> ppVar v
ppVarDecl (IFreeDecl v) = text "free" <+> ppVar v

--- Pretty print assignments
--- @param as the assignments
--- @return   the pretty printed assignments
ppAssignments :: [IAssign] -> Doc
ppAssignments = vcat . map ppAssignment

--- Pretty print an assignment
--- @param as the assignment
--- @return   the pretty printed assignment
ppAssignment :: IAssign -> Doc
ppAssignment (IVarAssign v e) = ppVar v <+> equals <+> ppExpr e
ppAssignment (INodeAssign v pos e) =
  ppVar v <> ppPos pos <+> equals <+> ppExpr e

--- Pretty print comma separated expressions
--- @param exprs the expressions
--- @return      the pretty printed expressions
ppExprs :: [IExpr] -> Doc
ppExprs = hsep . punctuate comma . map ppExpr

--- Pretty print an ICurry expression
--- @param expr the expression
--- @return     the pretty printed expression
ppExpr :: IExpr -> Doc
ppExpr (IVar v)           = ppVar v
ppExpr (IVarAccess v pos) = ppVar v <> ppPos pos
ppExpr (ILit l)           = ppLit l
ppExpr (IFCall n es)      = ppQName n <> parens (ppExprs es)
ppExpr (ICCall n es)      = ppExpr (IFCall n es)
ppExpr (IFPCall n _ es)   = ppQName n <> parens (ppExprs es)
ppExpr (ICPCall n m es)   = ppExpr (IFPCall n m es)
ppExpr (IOr e1 e2)        = parens (ppExpr e1 <+> char '?' <+> ppExpr e2)

--- Pretty print branches over constructors
--- @param bs the branches
--- @return   the pretty printed branches
ppConsBranches :: [IConsBranch] -> Doc
ppConsBranches = vcat . map ppConsBranch

--- Pretty print a branch over constructors
--- @param b the branch
--- @return  the pretty printed branch
ppConsBranch :: IConsBranch -> Doc
ppConsBranch (IConsBranch c ar block) =
  ppQName c <+> char '/' <+> int ar <+> rarrow <+> ppBlock block

--- Pretty print branches over literals
--- @param bs the branches
--- @return   the pretty printed branches
ppLitBranches :: [ILitBranch] -> Doc
ppLitBranches = vcat . map ppLitBranch

--- Pretty print a branch over literals
--- @param b the branch
--- @return  the pretty printed branch
ppLitBranch :: ILitBranch -> Doc
ppLitBranch (ILitBranch l block) = ppLit l <+> rarrow <+> ppBlock block

--- Pretty print an ICurry position
ppPos :: [Int] -> Doc
ppPos pos = text (show pos)

--- Pretty print an ICurry literal
--- @param lit the literal
--- @return    the pretty printed literal
ppLit :: ILiteral -> Doc
ppLit (IInt i)   = int i
ppLit (IFloat f) = float f
ppLit (IChar c)  = text (show c)

------------------------------------------------------------------------------
