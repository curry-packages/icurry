--- Types for representing ICurry programs
--- @author Marc Andre Wittorf

module ICurry.Types where

import FlatCurry.Types

--- An ICurry qualified name
type IQName = (String, String) -- ModuleName.LocalName

--- An ICurry arity
type IArity = Int

--- An ICurry variable (numerical index)
type IVarIndex = Int

--- An ICurry type variable (numerical index)
type ITVarIndex = Int

--- An ICurry assignment
type IAssign = (IVarIndex, IExpr) -- Variable = Expression

--- A Literal
--- @cons IInt   an integer literal
--- @cons IChar  a char literal
--- @cons IFloat a float literal
data ILiteral
  = IInt
      Int             -- the integer value
  | IChar
      Char            -- the char value
  | IFloat
      Float           -- the float value
  deriving (Show, Read)

--- An ICurry module
data IProg
  = IProg
      String          -- module name
      [String]        -- imports
      [IDataType]     -- declared data types
      [IFunction]     -- declared functions
  deriving (Show, Read)

--- An ICurry visibility
--- @cons Public  Visible and usable from other modules
--- @cons Private Invisible and not usable from other modules
data IVisibility
  = Public
  | Private
  deriving (Show, Read)

--- An ICurry data type
type IDataType = (
      IQName,         -- the data type's name
      IVisibility,    -- if this data type can be used from other modules
      [ITVarIndex],   -- type variables the type is parameterized over
      [IConstructor]) -- all possible constructors

--- An ICurry constructor
data IConstructor
  = IConstructor
      IQName          -- the constructor's name
      IVisibility     -- if this constructor can be used from other modules
      [ITExpr]        -- one type expression for each argument
  deriving (Show, Read)

--- An ICurry type expression
--- @cons ITVar  a type argument
--- @cons ITFunc a functional type
--- @cons ITCons a type application
data ITExpr
  = ITVar
      ITVarIndex      -- the type variable
  | ITFunc
      ITExpr          -- domain type
      ITExpr          -- range type
  | ITCons
      IQName          -- the type's name that is applied
      [ITExpr]        -- the arguments
  deriving (Show, Read)

--- An ICurry function
data IFunction
  = IFunction
      IQName          -- the function's name
      IVisibility     -- if this function can be used from other modules
      IFuncBody       -- what the function does
  deriving (Show, Read)

--- An ICurry function's behavior
--- @cons IExternal the function is externally defined
--- @cons IFuncBody the function is defined here
data IFuncBody
  = IExternal
      IArity          -- the function's arity
      String          -- the function's external name
  | IFuncBody
      [IVarIndex]     -- the function's arguments
      IBlock          -- the function's actual behavior
  deriving (Show, Read)

--- An ICurry block
--- @cons ISimpleBlock   unconditional evaluation
--- @cons ICaseConsBlock conditional evaluation over constructor terms
--- @cons ICaseLitBlock  conditional evaluation over literals
data IBlock
  = ISimpleBlock
      [IAssign]       -- assignments to local variables
      IExpr           -- the return expression
  | ICaseConsBlock
      [IAssign]       -- assignments to local variables
      IVarIndex       -- the variable to differentiate by
      [IConsBranch]   -- the possible branches
  | ICaseLitBlock
      [IAssign]       -- assignments to local variables
      IVarIndex       -- the variable to differentiate by
      [ILitBranch]    -- the possible branches
  deriving (Show, Read)

--- An ICurry branch over constructors
data IConsBranch
  = IConsBranch
      IQName          -- the constructor to match this branch
      [IVarIndex]     -- variable bindings
      IBlock          -- what happens if this branch is taken
  deriving (Show, Read)

--- An ICurry branch over literals
data ILitBranch
  = ILitBranch
      ILiteral        -- the literal to match this branch
      IBlock          -- what happens if this branch is taken
  deriving (Show, Read)

--- An ICurry expression
--- @cons IVar   a variable
--- @cons ILit   a literal
--- @cons IFCall a function call
--- @cons ICCall a constructor call
--- @cons IOr    a non-deterministic choice
data IExpr
  = IVar
      IVarIndex       -- the variable
  | ILit
      ILiteral        -- the literal's value
  | IFCall
      IQName          -- the function's name
      [IExpr]         -- the arguments
  | ICCall
      IQName          -- the constructor's name
      [IExpr]         -- the arguments
  | IOr
      [IExpr]         -- the possibilities
  deriving (Show, Read)

------------------------------------------------------------------------------
-- This is not part of the ICurry format.
-- It is needed to convert Typed FlatCurry to ICurry where
-- type information about imported modules is required.

--- A mapping from qualified function names to its signature and
--- a list of type variables it needs to be completely defined.
--- The type variables occur in the types of free variables used
--- in the function definition. Most implementations require
--- generators for logic variables. If these variables have
--- polymorphic types, generators must be passed as additional
--- arguments to implement such functions.
type NeededMapping = (
      QName,           -- a function's name
        (TypeExpr,     -- the function's signature
         [TVarIndex])) -- type variables (from the signature) for which
                       -- generators are required

------------------------------------------------------------------------------
