{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analysis (Analysis, AnalysisResult(..)) where

import Common (Lit (..), Name (..), Op (..), Pattern (..))
import Control.Applicative ((<|>))
import Control.Monad (foldM, when, unless)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), State, runState, gets)
import Data.Foldable (forM_)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Parsing (ParseResult(..))
import Presyntax (PDecl (..), PExpr (..), PProgram (..), PStmt (..), PStructField (..), PTy (..))
import Syntax (CaseBranch (..), Decl (..), Expr (..), Program (..), Stmt (..), Ty (..))
import Text.Megaparsec (SourcePos, initialPos)
import Errors (Error(..), Warning(..), CriticalError(..), Reportable (report))
import Foreign (Storable(alignment))

data AnalysisResult = AnalysisResult
  { errors :: [Error],
    warnings :: [Warning],
    currPos :: SourcePos,
    env :: Environment
  }

data GlobalScope = GlobalScope
  { functions :: Map.Map Name Ty,
    variables :: Map.Map Name Ty,
    structs :: Map.Map Name [(Name, Ty)],
    typeAliases :: Map.Map Name Ty,
    declarationPositions :: Map.Map Name SourcePos  -- Track positions of all declarations
  }

data Environment = Environment
  { globalScope :: GlobalScope,
    localScopes :: [Map.Map Name Ty]
  }

type Analysis a = ExceptT CriticalError (State AnalysisResult) a

class Type a where
  infer :: a -> Analysis Ty

instance Type PTy where
  infer :: PTy -> Analysis Ty
  infer (PTyName name) = case getName name of
    "Int" -> return TyInt
    "Float" -> return TyFloat
    "Bool" -> return TyBool
    "Char" -> return TyChar
    "String" -> return TyString
    "Void" -> return TyVoid
    _ -> do
      lookupType name
  infer (PTyArray pty) = do
    ty <- infer pty
    return $ TyArray ty
  infer (PTyFunction pty1 pty2) = do
    ty1 <- infer pty1
    ty2 <- infer pty2
    return $ TyFn ty1 ty2
  infer (PTyTuple ptys) = do
    tys <- mapM infer ptys
    return $ TyTuple tys

instance Type Lit where
  infer :: Lit -> Analysis Ty
  infer (LInt _) = return TyInt
  infer (LFloat _) = return TyFloat
  infer (LBool _) = return TyBool
  infer (LChar _) = return TyChar
  infer (LString _) = return TyString
  infer LNull = return TyUnknown

instance Type PExpr where
  infer :: PExpr -> Analysis Ty
  infer (PWithPos pos e) = do
    setPos pos
    infer e
  infer (PArrayExpr []) = return $ TyArray TyUnknown
  infer (PArrayExpr (x : xs)) = do
    ty <- infer x
    forM_ xs $ \y -> do
      ty' <- infer y
      pos <- gets currPos
      when (typeNeq ty ty') $ do
        addError $ TypeMismatch ty' ty "Array elements must have the same type" pos
    return $ TyArray ty
  infer (PTupleExpr []) = do
    pos <- gets currPos
    throwError $ ImpossibleError "Empty tuple" pos
  infer (PTupleExpr [_]) = do
    pos <- gets currPos
    throwError $ ImpossibleError "Singleton tuple" pos
  infer (PTupleExpr xs) = do
    types <- mapM infer xs
    return $ TyTuple types
  infer (PLitExpr lit) = infer lit
  infer (PVarExpr name) = undefined
  infer (PCallExpr expr args) = undefined
  infer (PBinaryExpr op e1 e2) = undefined
  infer (PUnaryExpr op e) = undefined
  infer (PLambdaExpr params expr) = undefined
  infer (PLambdaBlock params stmts) = undefined
  infer (PStructExpr name fields) = undefined
  infer (PFieldAccessExpr expr name) = undefined
  infer (PArrayAccessExpr arr idx) = undefined
  infer (PTupleAccessExpr expr idx) = undefined

typeNeq :: Ty -> Ty -> Bool
typeNeq TyUnknown _ = False
typeNeq _ TyUnknown = False
typeNeq a b = (/=) a b

addError :: Error -> Analysis ()
addError err = do
  analysisResult <- get
  let errors' = err : errors analysisResult
  put $ analysisResult { errors = errors' }

addWarning :: Warning -> Analysis ()
addWarning warn = do
  analysisResult <- get
  let warnings' = warn : warnings analysisResult
  put $ analysisResult { warnings = warnings' }

setPos :: SourcePos -> Analysis ()
setPos pos = do
  analysisResult <- get
  put $ analysisResult { currPos = pos }

lookupType :: Name -> Analysis Ty
lookupType name = do
  env <- gets env
  let scope = globalScope env
  let maybeType = Map.lookup name $ typeAliases scope
  let maybeStruct = Map.lookup name $ structs scope

  case maybeType of
    Just ty -> return ty
    Nothing -> case maybeStruct of
      Just _ -> return $ TyStruct name
      Nothing -> do
        pos <- gets currPos
        addError $ TypeNotFound name pos
        return TyUnknown

