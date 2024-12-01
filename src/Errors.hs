{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Errors
  ( Error (..),
    Warning (..),
    CriticalError (..),
    Reportable (..),
  )
where

import Common (Name (..))
import qualified Data.Vector as V
import Syntax (Ty (..))
import Text.Megaparsec (SourcePos, sourceColumn, sourceLine)
import Text.Megaparsec.Pos (unPos)

class Reportable e where
  -- | Format an error/warning with source lines context
  report :: V.Vector String -> e -> String

data CriticalError
  = ImpossibleError
      { msg :: String,
        sourcePos :: SourcePos
      }
  | DuplicateDeclaration
      { name :: Name,
        currentPos :: SourcePos,
        previousPos :: SourcePos -- Location of the original declaration
      }

instance Reportable CriticalError where
  report sourceLines = \case
    DuplicateDeclaration {..} -> do
      let currLineNum = unPos (sourceLine currentPos)
      let currCol = unPos (sourceColumn currentPos)
      let prevLineNum = unPos (sourceLine previousPos)
      let lineNumWidth = V.length sourceLines
      let currLine = sourceLines V.! (currLineNum - 1)
      let prevLine = sourceLines V.! (prevLineNum - 1)
      let nameLength = length (getName name)

      let errorText =
            [ replicate (lineNumWidth - length (show currLineNum)) ' ' ++ show currLineNum ++ " | " ++ currLine,
              replicate (lineNumWidth + 3 + currCol - 1) ' ' ++ replicate nameLength '^',
              "Duplicate Declaration: " ++ getName name ++ " is already declared here:",
              replicate (lineNumWidth - length (show prevLineNum)) ' ' ++ show prevLineNum ++ " | " ++ prevLine
            ]

      unlines errorText

    ImpossibleError {..} ->
      "Internal Error (Should never happen): " ++ msg

data Error
  = TypeMismatch
      { recv :: Ty,
        expected :: Ty,
        message :: String,
        sourcePos :: SourcePos
      }
  | TypeNotFound
      { name :: Name,
        sourcePos :: SourcePos
      }
  | VariableNotFound
      { name :: Name,
        sourcePos :: SourcePos
      }

-- instance Reportable Error where
--   report sourceLines = \case
--     TypeMismatch{..} ->
--       let line = sourceLine sourcePos
--           col = sourceColumn sourcePos
--           lineNumWidth = length (show line)
--           linePrefix = replicate lineNumWidth ' ' ++ " | "
--           errorLine = sourceLines !! (line - 1)
--           pointer = linePrefix ++ replicate (col - 1) ' ' ++ "^"
--       in
--       "\n" ++ show line ++ " | " ++ errorLine ++ "\n" ++
--       pointer ++ "\n" ++
--       "Type Mismatch: Expected " ++ show expected ++
--       " but got " ++ show recv ++ "\n" ++
--       message

--     TypeNotFound{..} ->
--       let line = sourceLine sourcePos
--           col = sourceColumn sourcePos
--           lineNumWidth = length (show line)
--           linePrefix = replicate lineNumWidth ' ' ++ " | "
--           errorLine = sourceLines !! (line - 1)
--           pointer = linePrefix ++ replicate (col - 1) ' ' ++ "^"
--       in
--       "\n" ++ show line ++ " | " ++ errorLine ++ "\n" ++
--       pointer ++ "\n" ++
--       "Type Not Found: " ++ getName name

data Warning
  = UnusedVariable
  { name :: Name,
    sourcePos :: SourcePos
  }
  | ShadowingWarning
  { name :: Name,
    sourcePos :: SourcePos
  }

-- instance Reportable Warning where
--   report sourceLines UnusedVariable{..} =
--     let line = sourceLine sourcePos
--         col = sourceColumn sourcePos
--         lineNumWidth = length (show line)
--         linePrefix = replicate lineNumWidth ' ' ++ " | "
--         errorLine = sourceLines !! (line - 1)
--         pointer = linePrefix ++ replicate (col - 1) ' ' ++ "^"
--     in
--     "\n" ++ show line ++ " | " ++ errorLine ++ "\n" ++
--     pointer ++ "\n" ++
--     "Warning: Unused variable " ++ getName name
