module Common (Filename, Name (..), Lit (..), Op (..), Pattern (..), capitalize, patternToString) where

import Data.Char (toUpper)

type Filename = String

newtype Name = Name
  { getName :: String }
  deriving (Eq, Show, Ord)

data Lit
  = LInt Integer
  | LFloat Double
  | LBool Bool
  | LChar Char
  | LString String
  | LNull
  deriving (Eq, Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Not
  | Minus
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | Concat
  deriving (Eq, Show)

data Pattern
  = BindPattern Name -- Simple variable binding: x
  | LitPattern Lit -- Literal patterns: 42, "hello", true
  | TuplePattern [Pattern] -- Tuple patterns: (x, y)
  | StructPattern Name [(Name, Pattern)] -- Struct patterns: Point { x: 42, y }
  | ArrayPattern [Pattern] -- Array patterns: [x, ...xs]
  | WildcardPattern -- Wildcard: _
  | RestPattern Name -- Rest pattern: ...xs, ...rest, ...tail etc.
  deriving (Eq, Show)

patternToString :: Pattern -> String
patternToString (BindPattern _) = "bind"
patternToString (LitPattern _) = "literal"
patternToString (TuplePattern _) = "tuple"
patternToString (StructPattern _ _) = "struct"
patternToString (ArrayPattern _) = "array"
patternToString WildcardPattern = "wildcard"
patternToString (RestPattern _) = "rest"

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs
