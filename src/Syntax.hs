module Syntax 
  ( Ty(..)
  , Program(..)
  , Decl(..)
  , Stmt(..)
  , CaseBranch(..)
  , Expr(..)
  ) where

import Common (Lit (..), Name (..), Op (..), Pattern(..))

newtype Program = Program [Decl] deriving (Eq, Show)

data Ty
  = TyInt
  | TyFloat 
  | TyBool
  | TyChar
  | TyString
  | TyCustom Name Ty
  | TyStruct Name
  | TyVoid
  | TyUnknown                
  | TyArray Ty
  | TyTuple [Ty]
  | TyFn Ty Ty                
  deriving (Eq, Show)

data Decl
  = FunDecl Name [Name] [Stmt] -- Functions
  | VarDecl Pattern (Maybe Expr)  -- Global variables
  | StructDecl Name [(Name, Ty)]     -- Structs
  deriving (Eq, Show)

data Stmt
  = VarStmt Pattern (Maybe Expr)
  | IfStmt Expr [Stmt] (Maybe [Stmt])
  | WhileStmt Expr [Stmt]
  | ForStmt Pattern Expr [Stmt]
  | CaseStmt Expr [CaseBranch]
  | ReturnStmt (Maybe Expr)
  | AssignStmt Expr Expr
  | ContinueStmt
  | BreakStmt
  | ExprStmt Expr
  deriving (Eq, Show)
  
data CaseBranch = PCaseBranch
  { branchPattern :: Pattern,
    guard :: Maybe Expr,                     -- if guard { ... }
    branchBody :: [Stmt]
  } deriving (Eq, Show)

data Expr
  = VarExpr Name
  | LitExpr Lit
  | CallExpr Expr [Expr]
  | BinaryExpr Op Expr Expr
  | UnaryExpr Op Expr
  | LambdaExpr [Name] [Stmt] 
  | StructExpr Name [(Name, Expr)]
  | FieldAccessExpr Expr Name
  | ArrayAccessExpr Expr Expr
  | TupleAccessExpr Expr Int
  deriving (Eq, Show)



