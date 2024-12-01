{-# LANGUAGE InstanceSigs #-}
module Pretty (Pretty (..), indent, prettifyStmts) where

import Common (Lit (..), Name (..), Op (..), Pattern(..))
import Data.List (intercalate)
import Presyntax
    ( PExpr(..),
      PCaseBranch(PCaseBranch),
      PStmt(..),
      PStructField(PStructField),
      PDecl(..),
      PTy(..),
      PProgram(..) )

class Pretty a where
  pretty :: a -> String

-- Names and Literals
instance Pretty Name where
  pretty :: Name -> String
  pretty (Name s) = s

instance Pretty Lit where
  pretty :: Lit -> String
  pretty lit = case lit of
    LInt n -> show n
    LFloat d -> show d
    LBool b -> if b then "true" else "false"
    LChar c -> show c
    LString s -> show s
    LNull -> "null"

-- Operators
instance Pretty Op where
  pretty :: Op -> String
  pretty op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
    And -> "&&"
    Or -> "||"
    Not -> "!"
    Minus -> "-"
    Eq -> "=="
    Neq -> "!="
    Lt -> "<"
    Gt -> ">"
    Lte -> "<="
    Gte -> ">="
    Concat -> "++"

-- Types
instance Pretty PTy where
  pretty :: PTy -> String
  pretty ty = case ty of
    PTyName name -> pretty name
    PTyArray t -> pretty t ++ "[]"
    PTyFunction t1 t2 ->
      let p1 = case t1 of
            PTyFunction {} -> "(" ++ pretty t1 ++ ")"
            _ -> pretty t1
       in p1 ++ " -> " ++ pretty t2
    PTyTuple ts -> "(" ++ intercalate ", " (map pretty ts) ++ ")"

-- Patterns
instance Pretty Pattern where
  pretty :: Pattern -> String
  pretty pat = case pat of
    BindPattern name -> pretty name
    LitPattern lit -> pretty lit
    TuplePattern pats ->
      "(" ++ intercalate ", " (map pretty pats) ++ ")"
    StructPattern name fields ->
      pretty name
        ++ " { "
        ++ intercalate ", " [pretty name' ++ ": " ++ pretty pat' | (name', pat') <- fields]
        ++ " }"
    ArrayPattern pats ->
      "[" ++ intercalate ", " (map pretty pats) ++ "]"
    WildcardPattern -> "_"
    RestPattern name -> "..." ++ pretty name

-- Expressions
instance Pretty PExpr where
  pretty :: PExpr -> String
  pretty expr = case expr of
    PVarExpr name -> pretty name
    PLitExpr lit -> pretty lit
    PCallExpr func args ->
      pretty func ++ "(" ++ intercalate ", " (map pretty args) ++ ")"
    PBinaryExpr op e1 e2 ->
      let p1 = case e1 of
            PBinaryExpr {} -> "(" ++ pretty e1 ++ ")"
            _ -> pretty e1
          p2 = case e2 of
            PBinaryExpr {} -> "(" ++ pretty e2 ++ ")"
            _ -> pretty e2
       in p1 ++ " " ++ pretty op ++ " " ++ p2
    PUnaryExpr op e -> pretty op ++ pretty e
    PLambdaExpr params expr' ->
      "\\("
        ++ intercalate ", " [pretty name ++ ": " ++ pretty ty | (name, ty) <- params]
        ++ ") -> "
        ++ pretty expr'
    PLambdaBlock params stmts ->
      "\\("
        ++ intercalate ", " [pretty name ++ ": " ++ pretty ty | (name, ty) <- params]
        ++ ") -> "
        ++ "{\n"
        ++ indent (prettifyStmts stmts)
        ++ "}"
    PStructExpr name fields ->
      pretty name
        ++ " { "
        ++ intercalate ", " [pretty name' ++ " = " ++ pretty expr' | (name', expr') <- fields]
        ++ " }"
    PFieldAccessExpr expr' name ->
      pretty expr' ++ "." ++ pretty name
    PArrayAccessExpr arr idx ->
      pretty arr ++ "[" ++ pretty idx ++ "]"
    PTupleAccessExpr expr' idx ->
      pretty expr' ++ "." ++ show idx
    PArrayExpr exprs ->
      "[" ++ intercalate ", " (map pretty exprs) ++ "]"
    PTupleExpr exprs ->
      "(" ++ intercalate ", " (map pretty exprs) ++ ")"
    PWithPos _ e -> pretty e -- Ignore position information in pretty printing

-- Statements
instance Pretty PStmt where
  pretty :: PStmt -> String
  pretty stmt = case stmt of
    PVarStmt pat ty expr ->
      "let "
        ++ pretty pat
        ++ maybe "" (\t -> ": " ++ pretty t) ty
        ++ maybe "" (\e -> " = " ++ pretty e) expr
        ++ ";"
    PIfStmt cond thenStmts elseStmts ->
      "if "
        ++ pretty cond
        ++ " {\n"
        ++ indent (prettifyStmts thenStmts)
        ++ "}"
        ++ maybe "" (\s -> " else {\n" ++ indent (prettifyStmts s) ++ "}") elseStmts
    PWhileStmt cond body ->
      "while "
        ++ pretty cond
        ++ " {\n"
        ++ indent (prettifyStmts body)
        ++ "}"
    PForStmt pat expr body ->
      "for "
        ++ pretty pat
        ++ " in "
        ++ pretty expr
        ++ " {\n"
        ++ indent (prettifyStmts body)
        ++ "}"
    PCaseStmt expr branches ->
      "case "
        ++ pretty expr
        ++ " {\n"
        ++ indent (intercalate "\n" (map pretty branches))
        ++ "}"
    PReturnStmt expr -> do
      case expr of
        Just expr' -> "return " ++ pretty expr' ++ ";"
        Nothing -> "return;"
    PAssignStmt lhs rhs ->
      pretty lhs ++ " = " ++ pretty rhs ++ ";"
    PExprStmt expr ->
      pretty expr ++ ";"
    PContinueStmt -> "continue;"
    PBreakStmt -> "break;"
    PWithPosStmt _ s -> pretty s 

-- Case branches
instance Pretty PCaseBranch where
  pretty :: PCaseBranch -> String
  pretty (PCaseBranch pat guard' body) =
    pretty pat
      ++ maybe "" (\g -> " if " ++ pretty g) guard'
      ++ " => {\n"
      ++ indent (prettifyStmts body)
      ++ "}"

instance Pretty PStructField where
  pretty :: PStructField -> String
  pretty (PStructField name ty) = pretty name ++ ": " ++ pretty ty ++ ";"

-- Declarations
instance Pretty PDecl where
  pretty :: PDecl -> String
  pretty decl = case decl of
    PFunDecl name params retTy body ->
      "fn "
        ++ pretty name
        ++ "("
        ++ intercalate ", " [pretty name' ++ ": " ++ pretty ty | (name', ty) <- params]
        ++ ")"
        ++ maybe "" (\t -> " -> " ++ pretty t) retTy
        ++ " {\n"
        ++ indent (prettifyStmts body)
        ++ "}"
    PTypeDecl name ty ->
      "type " ++ pretty name ++ " = " ++ pretty ty ++ ";"
    PStructDecl name fields ->
      "struct "
        ++ pretty name
        ++ " {\n"
        ++ indent (unlines [pretty name' ++ ": " ++ pretty ty ++ "," | PStructField name' ty <- fields])
        ++ "}"
    PVarDecl name ty expr ->
      "let "
        ++ pretty name
        ++ maybe "" (\t -> ": " ++ pretty t) ty
        ++ maybe "" (\e -> " = " ++ pretty e) expr
        ++ ";"
    PWithPosDecl _ d -> pretty d

-- Program
instance Pretty PProgram where
  pretty :: PProgram -> String
  pretty (PProgram decls) = intercalate "\n" (map pretty decls)

-- Helper functions
prettifyStmts :: [PStmt] -> String
prettifyStmts = intercalate "\n" . map pretty

-- For proper indentation
indent :: String -> String
indent = unlines . map ("    " ++) . lines