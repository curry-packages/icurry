------------------------------------------------------------------------------
--- This library contains type definitions to represent ICurry programs.
------------------------------------------------------------------------------

module ICurry.Types where

--- Each name in ICurry consists of the module name, the local name
--- inside a module, and a number. For constructors, the number
--- is the index of the constructors in the data type definition,
--- i.e., all constructors of a data type are numbered from 0
--- (to support efficient switch statements for pattern matching).
--- For functions, the number is unique inside a module so that
--- functions can be efficiently accessed inside a module.
type IQName = (String, String, Int)

--- An arity of ICurry functions and constructors is just an integer.
type IArity = Int

--- An ICurry variable is represented by a index which must be unique
--- inside a function declaration.
type IVarIndex = Int

--- A literal occurring in expressions.
--- @cons IInt   an integer literal
--- @cons IChar  a char literal
--- @cons IFloat a float literal
data ILiteral =
    IInt   Int             -- the integer value
  | IChar  Char            -- the char value
  | IFloat Float           -- the float value
  deriving (Show, Read)

--- An ICurry module consists of a module name, the names of imported modules,
--- and data type and function declarations.
data IProg = IProg String [String] [IDataType] [IFunction]
  deriving (Show, Read)

--- An ICurry data type. Since type information is not relevant here,
--- it is represented by the name of the data type together with
--- all constructors names with their arities.
data IDataType = IDataType IQName [(IQName,IArity)]
  deriving (Show, Read)

--- An ICurry function declaration consisting of the function's name,
--- arity, visibility, the positions of always demandeded arguments
--- (where 0 denotes the first argument), and a body.
--- Note that the demanded arguments are definitely required to
--- evaluate the function. In some situations (e.g., complex nested case
--- statements), more arguments might be demanded.
data IFunction = IFunction IQName IArity IVisibility [Int] IFuncBody
  deriving (Show, Read)

--- The visibility of ICurry entities.
--- @cons Public  - visible and usable from other modules
--- @cons Private - invisible and not usable from other modules
data IVisibility = Public | Private
  deriving (Show, Read)

--- The body specifying the behavior of an ICurry function.
--- @cons IExternal - the function is externally defined
--- @cons IFuncBody - the function is defined by a block
data IFuncBody =
    IExternal String          -- the function's external name
  | IFuncBody IBlock          -- the function's actual behavior
  deriving (Show, Read)

--- An ICurry block. Each block consists of variable declarations,
--- assignments, and a statement.
data IBlock = IBlock [IVarDecl] [IAssign] IStatement
  deriving (Show, Read)

--- An ICurry variable declaration declares either a local variable
--- or a free variable.
--- In the subsequent assignments, graph nodes will be assigned to
--- these variables. Instead of explicitly handling free variables,
--- one could also use local variables and assign a node representing
--- a value generator operation for this free variable.
--- Since different implementations might use different strategies
--- to deal with free variables, ICurry supports both options.
---
--- NOTE: The ICurry code assumes that the ICurry variable with index 0
--- always points to the root of the left-hand side when an ICurry function
--- is invoked. This invariant must be ensured by the ICurry application!
data IVarDecl = IVarDecl  IVarIndex
              | IFreeDecl IVarIndex
  deriving (Show, Read)

--- An ICurry assignment is either an assignment to a local variable
--- or an assignment to a successor of a node.
data IAssign = IVarAssign  IVarIndex IExpr       -- var = exp
             | INodeAssign IVarIndex [Int] IExpr -- var[...] = exp
  deriving (Show, Read)

--- An ICurry statement.
--- A return statement constructs a graph that replaces the current call.
--- An exempt or failure statement terminates the current task.
--- A case statement on constructors is assumed to be complete,
--- i.e., there is a branch for each constructor of the type of
--- the case argument. Furthermore, the branches should have the
--- order of the constructors.
--- For case statements on literals, there is no such restriction.
--- @cons IReturn - return statement
--- @cons IExempt - failure statement
--- @cons ICaseConsBlock - conditional evaluation over constructor terms
--- @cons ICaseLitBlock  - conditional evaluation over literals
data IStatement =
    IExempt
  | IReturn IExpr
  | ICaseCons IVarIndex [IConsBranch]
  | ICaseLit  IVarIndex [ILitBranch]
  deriving (Show, Read)

--- An ICurry case branch over algebraic constructors.
--- Only the constructor and its arity matching this branch is given.
--- The assignments of constructor arguments to pattern variables
--- must be done in the ICurry block.
data IConsBranch = IConsBranch IQName IArity IBlock
  deriving (Show, Read)

--- An ICurry case branch over literals.
data ILitBranch = ILitBranch ILiteral IBlock
  deriving (Show, Read)

--- An ICurry expression.
--- @cons IVar       - a variable
--- @cons IVarAccess - an access to a node argument
--- @cons ILit       - a literal
--- @cons IFCall     - a function call
--- @cons ICCall     - a constructor call
--- @cons IFPCall    - a partial function call with number of missing args
--- @cons ICPCall    - a partial constructor call with number of missing args
--- @cons IOr        - a non-deterministic choice
data IExpr
  = IVar        IVarIndex           -- the variable
  | IVarAccess  IVarIndex [Int]     -- access to node arguments
  | ILit        ILiteral            -- the literal's value
  | IFCall      IQName [IExpr]      -- function call
  | ICCall      IQName [IExpr]      -- constructor call
  | IFPCall     IQName Int [IExpr]  -- function call
  | ICPCall     IQName Int [IExpr]  -- constructor call
  | IOr         IExpr IExpr         -- choice expression
  deriving (Show, Read)

------------------------------------------------------------------------------
