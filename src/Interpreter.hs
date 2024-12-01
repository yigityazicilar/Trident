module Interpreter () where

data Mode
  = -- Typecheck a file
    CheckFile String
  | -- Parse a file
    ParseFile String
  | -- Run a program
    Run String
  deriving (Show)

newtype Flags = Flags
  { verbose :: Bool
  }
  deriving (Show)

data Args = Args
  { mode :: Mode,
    flags :: Flags
  }
  deriving (Show)
