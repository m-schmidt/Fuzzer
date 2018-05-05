module Loop
  ( Loop(..)
  ) where


-- |Specification for a loop
data Loop = Loop LoopType         -- ^ Type of loop
                 CounterType      -- ^ Type of loop counter
                 ConditionType    -- ^ Exit condition
                 Constant         -- ^ Start value for loop counter
                 Constant         -- ^ Increment value for loop counter
                 Constant         -- ^ Comparison value for exit condition
                 Integer          -- ^ Number of iterations of the loop body
                 deriving (Eq, Show)


-- |Type of loop
data LoopType
  = Do
  | For
  | While
  deriving (Eq, Enum, Bounded, Ord, Show)


-- |C datatypes for loop counter specifying signedness and bit width
data CounterType
  = Signed   Integer
  | Unsigned Integer
  deriving (Eq, Show)


-- |C comparison operators for exit condition of loop
data ConditionType
  = LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | Equal
  | NotEqual
  deriving (Eq, Enum, Bounded, Ord, Show)


-- |Specification for an immediate constant
data Constant = Constant CounterType Integer
  deriving (Eq, Show)
