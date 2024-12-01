module Presyntax (PTy(..), PDecl(..), PStmt(..), PExpr(..), PProgram(..), PStructField(..), PCaseBranch(..)) where

import Common (Lit (..), Name (..), Op, Pattern(..))
import Text.Megaparsec (SourcePos (..))

newtype PProgram = PProgram [PDecl] deriving (Eq, Show)

data PTy 
  = PTyName Name        -- Basic types like Int, String
  | PTyArray PTy        -- Array types like Int[] or Int[][]
  | PTyFunction PTy PTy -- Function types like Int -> Int
  | PTyTuple [PTy]      -- Tuple types like (Int, String)
  deriving (Eq, Show)

data PDecl
  = PFunDecl Name [(Name, PTy)] (Maybe PTy) [PStmt]      
  | PVarDecl Pattern (Maybe PTy) (Maybe PExpr)          
  | PTypeDecl Name PTy                                   
  | PStructDecl Name [PStructField]                      
  | PWithPosDecl SourcePos PDecl                        -- Add position wrapper
  deriving (Eq, Show)

data PStructField = PStructField
  { fieldName :: Name,
    fieldType :: PTy
  } deriving (Eq, Show)

data PStmt
  = PVarStmt Pattern (Maybe PTy) (Maybe PExpr)       
  | PIfStmt PExpr [PStmt] (Maybe [PStmt])     
  | PWhileStmt PExpr [PStmt]                  
  | PForStmt Pattern PExpr [PStmt]           
  | PCaseStmt PExpr [PCaseBranch]             
  | PReturnStmt (Maybe PExpr)                   
  | PAssignStmt PExpr PExpr                   
  | PContinueStmt                             
  | PBreakStmt                                
  | PExprStmt PExpr
  | PWithPosStmt SourcePos PStmt                      -- Add position wrapper
  deriving (Eq, Show)
  
data PCaseBranch = PCaseBranch
  { branchPattern :: Pattern,
    guard :: Maybe PExpr,                     -- if guard { ... }
    branchBody :: [PStmt]
  } deriving (Eq, Show)

data PExpr
  = PVarExpr Name                                     -- Variables                x
  | PLitExpr Lit                                      -- Literals                 1, "hello", true, 'a', 1.0
  | PCallExpr PExpr [PExpr]                           -- Function calls           f(1, 2, 3)
  | PBinaryExpr Op PExpr PExpr                        -- Binary operations        1 + 2 * 3, a && b, [1, 2] ++ [3, 4]
  | PUnaryExpr Op PExpr                               -- Unary operations         -(1), !true
  | PLambdaExpr [(Name, PTy)] PExpr                   -- Single-line lambda       \(x: T) -> x
  | PLambdaBlock [(Name, PTy)] [PStmt]                -- Multi-line lambda block  \(x: T) -> { ... }
  | PStructExpr Name [(Name, PExpr)]                  -- Struct literals          Point { x = 1, y = 2 }
  | PFieldAccessExpr PExpr Name                       -- Field access             x.y
  | PArrayAccessExpr PExpr PExpr                      -- Array access             a[0]
  | PTupleAccessExpr PExpr Int                        -- Tuple access             t.0
  | PArrayExpr [PExpr]                                -- Array literals           [1, 2, 3]
  | PTupleExpr [PExpr]                                -- Tuple literals           (1, 2, 3)
  | PWithPos SourcePos PExpr                          -- Source positions         (for error messages)
  deriving (Eq, Show)