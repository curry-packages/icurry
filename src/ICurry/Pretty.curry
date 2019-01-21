--- Pretty print ICurry programs
--- @author Marc Andre Wittorf

module ICurry.Pretty where

import ICurry.Types
import Text.Pretty

--- Pretty print an ICurry module
--- @param prog the module
--- @return     the pretty printed module
ppIProg :: IProg -> Doc
ppIProg (IProg name imports types funcs) = vsepBlank
  [ ppHeader name
  , ppImports imports
  , ppTypes types
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
ppTypes :: [IDataType] -> Doc
ppTypes = vsepBlank . map ppType

--- Pretty print an ICurry type
--- @param dt the data type declaration
--- @return   the pretty printed type
ppType :: IDataType -> Doc
ppType (name, _, tvs, cs) = nest 1 $
  text "data" <+> ppQName name <+> ppTVars tvs $$ ppConstructors cs

--- Pretty print ICurry constructors
--- @param cs the data constructors
--- @return   the pretty printed constructors
ppConstructors :: [IConstructor] -> Doc
ppConstructors cs = vsep $ zipWith (<+>) (equals : repeat bar)
                                         (map ppConstructor cs)

--- Pretty print an ICurry constructor
--- @param c the data constructor
--- @return  the pretty printed constructor
ppConstructor :: IConstructor -> Doc
ppConstructor (IConstructor name _ es) = hsep $
  ppQName name : map (ppTExpr 'a') es

--- Pretty print ICurry functions
--- @param fns the functions
--- @return    the pretty printed functions
ppFunctions :: [IFunction] -> Doc
ppFunctions = vsepBlank . map ppFunction

--- Pretty print an ICurry function
--- @param fns the function
--- @return    the pretty printed function
ppFunction :: IFunction -> Doc
ppFunction (IFunction name _ body) = ppQName name <+> ppFuncBody body

--- Pretty print a qualified ICurry name (module.localname)
--- @param name the name
--- @return     the pretty printed name
ppQName :: IQName -> Doc
ppQName (modname, localname) = text $ modname ++ '.' : localname

--- Pretty print an ICurry function's argument list
--- @param arity the number of variables (1.. is assumed)
--- @return      the pretty printed argument list
ppArity :: IArity -> Doc
ppArity arity = hsep $ map ppVar [1 .. arity]

--- Pretty print an ICurry function's body
--- @param body the function's body
--- @return     the pretty printed function body
ppFuncBody :: IFuncBody -> Doc
ppFuncBody (IExternal arity name) = text ("external \"" ++ name ++ "\",")
                                    <+> int arity
                                    <+> text "args"
ppFuncBody (IFuncBody vars block) = ppVars vars <+> equals <+> ppBlock block

--- Pretty print a list of variables
--- @param vs the variables
--- @return   the pretty printed variables
ppVars :: [IVarIndex] -> Doc
ppVars = hsep . map ppVar

--- Pretty print a variable. Variables are called x0, x1, ...
--- @param v the variable
--- @return  the pretty printed variable
ppVar :: IVarIndex -> Doc
ppVar = text . ('x':) . show

--- Pretty print type variables
--- @param tvs the type variables
--- @return    the pretty printed type variables
ppTVars :: [ITVarIndex] -> Doc
ppTVars = hsep . map (ppTVar 'a')

--- Pretty print a type variable
--- @param c  the character to denote the variable
--- @param tv the type variable
--- @return   the pretty printed type variable
ppTVar :: Char -> ITVarIndex -> Doc
ppTVar c = text . (c:) . show

--- Pretty print a type expression
--- @param c  the character to denote type variables
--- @param te the type expression
--- @return   the pretty pritned type expression
ppTExpr :: Char -> ITExpr -> Doc
ppTExpr c (ITVar v) = ppTVar c v
ppTExpr c (ITFunc e1 e2) = parens (ppTExpr c e1)
                           <+> rarrow <+>
                           parens (ppTExpr c e2)
ppTExpr c (ITCons n es) = parens $ ppQName n <+> hsep (map (ppTExpr c) es)

--- Pretty print an ICurry block
--- @param block the block
--- @return      the rendered block
ppBlock :: IBlock -> Doc
ppBlock (ISimpleBlock a e) = nest 2 (
  lbrace $$
  ppLocals (map fst a) $$
  ppAssignments a $$
  text "return" <+>
  ppExpr e) $$ rbrace
ppBlock (ICaseLitBlock a v bs) = nest 2 (
  lbrace $$
  ppLocals (map fst a) $$
  ppAssignments a $$
  ppLitBranches bs $$
  text "case" <+> ppVar v <+> text "of") $$
  rbrace
ppBlock (ICaseConsBlock a v bs) = nest 2 (
  lbrace $$
  ppLocals (map fst a) $$
  ppAssignments a $$
  nest 2 (text "case" <+> ppVar v <+> text "of" $$
  ppConsBranches bs)) $$
  rbrace

--- Pretty print local variable declarations
--- @param vs the local variables
--- @return   the pretty printed variable declarations
ppLocals :: [IVarIndex] -> Doc
ppLocals ls = if null ls
                 then empty
                 else text "local"
                      <+> (hsep $ punctuate comma $ map ppVar ls)

--- Pretty print assignments
--- @param as the assignments
--- @return   the pretty printed assignments
ppAssignments :: [IAssign] -> Doc
ppAssignments = vcat . map ppAssignment

--- Pretty print an assignment
--- @param as the assignment
--- @return   the pretty printed assignment
ppAssignment :: IAssign -> Doc
ppAssignment (v, e) = ppVar v <+> equals <+> ppExpr e

--- Pretty print comma separated expressions
--- @param exprs the expressions
--- @return      the pretty printed expressions
ppExprs :: [IExpr] -> Doc
ppExprs = hsep . punctuate comma . map ppExpr

--- Pretty print an ICurry expression
--- @param expr the expression
--- @return     the pretty printed expression
ppExpr :: IExpr -> Doc
ppExpr (IVar v)      = ppVar v
ppExpr (ILit l)      = ppLit l
ppExpr (IFCall n es) = ppQName n <+> lparen <+> ppExprs es <+> rparen
ppExpr (ICCall n es) = text "<constructor>" <+> ppExpr (IFCall n es)
ppExpr (IOr es) = char '?' <+> lparen <+> ppExprs es <+> rparen

--- Pretty print an ICurry literal
--- @param lit the literal
--- @return    the pretty printed literal
ppLit :: ILiteral -> Doc
ppLit (IInt v)   = int v
ppLit (IFloat v) = float v
ppLit (IChar v)  = char v

--- Pretty print branches over literals
--- @param bs the branches
--- @return   the pretty printed branches
ppLitBranches :: [ILitBranch] -> Doc
ppLitBranches = hsep . map ppLitBranch

--- Pretty print a branch over literals
--- @param b the branch
--- @return  the pretty printed branch
ppLitBranch :: ILitBranch -> Doc
ppLitBranch (ILitBranch l block) = ppLit l <+> rarrow

--- Pretty print branches over constructors
--- @param bs the branches
--- @return   the pretty printed branches
ppConsBranches :: [IConsBranch] -> Doc
ppConsBranches = vcat . map ppConsBranch

--- Pretty print a branch over constructors
--- @param b the branch
--- @return  the pretty printed branch
ppConsBranch :: IConsBranch -> Doc
ppConsBranch (IConsBranch c vs block) =
  ppQName c <+> hsep (map ppVar vs) <+> rarrow <+> ppBlock block
