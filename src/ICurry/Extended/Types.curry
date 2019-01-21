--- Types for representing ICurry programs
--- @author Marc Andre Wittorf
module ICurry.Extended.Types where

import ICurry.Types

--- An Extended assignment
type IEAssign = (IVarIndex, IEExpr) -- Variable = Expression

--- An Extended module
data IEProg
  = IEProg
      String           -- module name
      [String]         -- imports
      [IEDataType]     -- declared data types
      [IEFunction]     -- declared functions
  deriving (Show, Read)

type IEQName = (IQName, Int) -- name and a module-unique number to represent
                             -- this name

--- An Extended visibility
--- @cons Public  Visible and usable from other modules
--- @cons Private Invisible and not usable from other modules
data IEVisibility
  = Public             -- usable from other modules
  | Private            -- usable only in this module
  deriving (Show, Read)

--- An Extended data type
type IEDataType = (
      IEQName,         -- the data type's name
      IEVisibility,    -- if this data type can be used from other modules
      [ITVarIndex],    -- type variables the type is parameterized over
      [IEConstructor]) -- all possible constructors

--- An Extended constructor
data IEConstructor
  = IEConstructor
      IEQName          -- the constructor's name
      IEVisibility     -- if this constructor can be used from other modules
      [IETExpr]        -- one type expression for each argument
  deriving (Show, Read)

--- An Extended type expression
--- @cons IETVar  a type argument
--- @cons IETFunc a functional type
--- @cons IETCons a type application
data IETExpr
  = IETVar
      ITVarIndex       -- the type variable
  | IETFunc
      IETExpr          -- domain type
      IETExpr          -- range type
  | IETCons
      IEQName          -- the type's name that is applied
      [IETExpr]        -- the arguments
  deriving (Show, Read)

--- An Extended function
data IEFunction
  = IEFunction
      IEQName          -- the function's name
      IEVisibility     -- if this function can be used from other modules
      IEFuncBody       -- what the function does
  deriving (Show, Read)

--- An Extended function's behavior
--- @cons IEExternal the function is externally defined
--- @cons IEFuncBody the function is defined here
data IEFuncBody
  = IEExternal
      IArity           -- the function's arity
      String           -- the function's external name
  | IEFuncBody
      [IVarIndex]      -- the function's arguments
      IEBlock          -- the function's actual behavior
  deriving (Show, Read)

--- An Extended block
--- @cons IESimpleBlock   unconditional evaluation
--- @cons IECaseConsBlock conditional evaluation over constructor terms
--- @cons IECaseLitBlock  conditional evaluation over literals
data IEBlock
  = IESimpleBlock
      [IEAssign]       -- assignments to local variables
      IEExpr           -- the return expression
  | IECaseConsBlock
      [IEAssign]       -- assignments to local variables
      IVarIndex        -- the variable to differentiate by
      [IEConsBranch]   -- the possible branches
  | IECaseLitBlock
      [IEAssign]       -- assignments to local variables
      IVarIndex        -- the variable to differentiate by
      [IELitBranch]    -- the possible branches
  deriving (Show, Read)

--- An Extended branch over constructors
data IEConsBranch
  = IEConsBranch
      IEQName          -- the constructor to match this branch
      [IVarIndex]      -- variable bindings
      IEBlock          -- what happens if this branch is taken
  deriving (Show, Read)

--- An Extended branch over literals
data IELitBranch
  = IELitBranch
      ILiteral         -- the literal to match this branch
      IEBlock          -- what happens if this branch is taken
  deriving (Show, Read)

--- An Extended expression
--- @cons IEVar   a variable
--- @cons IELit   a literal
--- @cons IEFCall a function call
--- @cons IECCall a constructor call
--- @cons IEOr    a non-deterministic choice
data IEExpr
  = IEVar
      IVarIndex        -- the variable
  | IELit
      ILiteral         -- the literal's value
  | IEFCall
      IEQName          -- the function's name
      [IEExpr]         -- the arguments
  | IECCall
      IEQName          -- the constructor's name
      [IEExpr]         -- the arguments
  | IEOr
      [IEExpr]         -- the possibilities
  deriving (Show, Read)
