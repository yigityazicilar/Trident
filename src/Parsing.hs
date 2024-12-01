{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module Parsing (parseProgram, ParseError, ParseResult(..)) where

import Common (Filename, Lit (..), Name (..), Op (..), Pattern(..), capitalize, patternToString)
import Control.Monad (void, when)
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, InfixN, InfixR, Postfix, Prefix),
    makeExprParser,
  )
import Presyntax
  ( PCaseBranch (..),
    PDecl (..),
    PExpr (..),
    PProgram (..),
    PStmt (..),
    PStructField (..),
    PTy (..),
  )
import Pretty (pretty)
import Text.Megaparsec
  ( MonadParsec (lookAhead),
    ParseErrorBundle,
    Parsec,
    ShowErrorComponent (..),
    between,
    choice,
    customFailure,
    eof,
    getOffset,
    getSourcePos,
    many,
    manyTill,
    optional,
    parse,
    region,
    sepBy,
    sepBy1,
    sepEndBy1,
    setErrorOffset,
    try,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector as V

data ParseError
  = KeywordAsIdent String
  | VarHasNoTypeAndExpr String Int
  | VariableArrayPatternEmpty
  | MoreThanOneRestPattern String Int
  | UnexpectedTypeInPattern String String Int
  | PatternHasNoExpr String String Int
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ParseError where
  showErrorComponent :: ParseError -> String
  showErrorComponent (KeywordAsIdent n) = "Keyword '" ++ n ++ "' cannot be used as an identifier"
  showErrorComponent (VarHasNoTypeAndExpr n _) = "Variable '" ++ n ++ "' must have a type or an expression"
  showErrorComponent VariableArrayPatternEmpty = "Variables cannot have an empty array pattern"
  showErrorComponent (MoreThanOneRestPattern n _) = "Pattern '" ++ n ++ "' contains multiple rest patterns (...), but only one is allowed"
  showErrorComponent (UnexpectedTypeInPattern t n _) = "Type annotation not allowed in " ++ t ++ " pattern '" ++ n ++ "'"
  showErrorComponent (PatternHasNoExpr t n _) = capitalize t ++ " pattern '" ++ n ++ "' must be assigned a value"
  errorComponentLen :: ParseError -> Int
  errorComponentLen (KeywordAsIdent n) = length n
  errorComponentLen (VarHasNoTypeAndExpr _ l) = l
  errorComponentLen VariableArrayPatternEmpty = 2
  errorComponentLen (MoreThanOneRestPattern _ l) = l
  errorComponentLen (UnexpectedTypeInPattern _ _ l) = l
  errorComponentLen (PatternHasNoExpr _ _ l) = l

type Parser = Parsec ParseError String

-- Basic parser utilities
ws :: Parser ()
ws = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: String -> Parser String
symbol = L.symbol ws

symbol' :: String -> Parser ()
symbol' = void . symbol

reserved :: String -> Parser ()
reserved w = void (lexeme (string w))

-- Delimiters
parens :: Parser a -> Parser a
parens = between (symbol' "(") (symbol' ")")

braces :: Parser a -> Parser a
braces = between (symbol' "{") (symbol' "}")

brackets :: Parser a -> Parser a
brackets = between (symbol' "[") (symbol' "]")

comma :: Parser String
comma = symbol ","

sepByComma :: Parser a -> Parser [a]
sepByComma = (`sepBy` comma)

-- Reserved keywords
reservedWords :: [String]
reservedWords =
  [ "let",
    "fn",
    "if",
    "else",
    "while",
    "for",
    "in",
    "return",
    "case",
    "struct",
    "type",
    "true",
    "false",
    "null",
    "_",
    "..."
  ]

failure :: ParseError -> Parser a
failure = customFailure

-- Identifier parser
identifier :: Parser Name
identifier = lexeme $ do
  offset <- getOffset
  name <-
    (:)
      <$> (letterChar <|> char '_')
      <*> many (alphaNumChar <|> char '_')
  if name `elem` reservedWords
    then region (setErrorOffset offset) (failure $ KeywordAsIdent name)
    else return $ Name name

-- Literal parser
literal :: Parser Lit
literal =
  choice
    [ LNull <$ reserved "null",
      LInt <$> lexeme L.decimal,
      try $ LFloat <$> lexeme L.float,
      try $ LString <$> regularStringLiteral,
      try $ LChar <$> charLiteral,
      LBool <$> (True <$ reserved "true" <|> False <$ reserved "false")
    ]
  where
    regularStringLiteral = lexeme $ char '"' *> manyTill L.charLiteral (char '"')
    charLiteral = lexeme $ char '\'' *> L.charLiteral <* char '\''

-- Type parser
type_ :: Parser PTy
type_ = makeExprParser typeWithArrays [[InfixR (PTyFunction <$ symbol "->")]]

typeWithArrays :: Parser PTy
typeWithArrays = do
  baseType <- typeAtom
  arrayDims <- many (symbol "[]")
  return $ foldr (\_ t -> PTyArray t) baseType arrayDims

typeAtom :: Parser PTy
typeAtom =
  choice
    [ PTyName <$> identifier,
      try $ PTyTuple <$> parens (sepByComma type_),
      parens type_
    ]

-- Pattern parser
pattern_ :: Bool -> Parser Pattern
pattern_ isTopLayer =
  choice
    [ wildcardPattern,
      arrayPattern,
      try $ tuplePattern (pattern_ False) isTopLayer,
      litPattern,
      try structPattern,
      bindPattern
    ]

forPattern :: Bool -> Parser Pattern
forPattern isTopLayer =
  choice
    [ try $ tuplePattern (forPattern False) isTopLayer,
      bindPattern
    ]

varPattern :: Bool -> Parser Pattern
varPattern isTopLayer =
  choice
    [ wildcardPattern,
      arrayPatternNotEmpty,
      try $ tuplePattern (varPattern False) isTopLayer,
      bindPattern
    ]

tuplePattern :: Parser Pattern -> Bool -> Parser Pattern
tuplePattern p isTopLayer =
  if isTopLayer
    then parens tuple <|> tuple
    else parens tuple
  where
    tuple = do
      pat <- p
      symbol' ","
      pats <- p `sepBy1` symbol' ","
      return $ TuplePattern (pat : pats)

restPattern :: Parser Pattern
restPattern = do
  symbol' "..."
  RestPattern <$> identifier

wildcardPattern :: Parser Pattern
wildcardPattern = symbol' "_" >> return WildcardPattern

arrayPattern :: Parser Pattern
arrayPattern = do
  offset <- getOffset
  brackets $ do
    pat <- optional $ pattern_ False
    case pat of
      Nothing -> return $ ArrayPattern []
      Just p -> arrayPattern' (pattern_ False) p offset

arrayPatternNotEmpty :: Parser Pattern
arrayPatternNotEmpty = do
  offset <- getOffset
  brackets $ do
    pat <- optional $ varPattern False
    case pat of
      Nothing -> region (setErrorOffset offset) (failure VariableArrayPatternEmpty)
      Just p -> arrayPattern' (varPattern False) p offset

arrayPattern' :: Parser Pattern -> Pattern -> Int -> Parser Pattern
arrayPattern' parser p offset = do
  cont <- optional $ symbol' ","
  case cont of
    Nothing -> return $ ArrayPattern [p]
    Just _ -> do
      pats <- (restPattern <|> parser) `sepBy` symbol' ","
      let arrayP = ArrayPattern (p : pats)
      end <- getOffset
      when (countRestPattern pats > 1) $ region (setErrorOffset offset) (failure $ MoreThanOneRestPattern (pretty arrayP) (end - offset + 1))
      return arrayP

countRestPattern :: [Pattern] -> Int
countRestPattern [] = 0
countRestPattern (p : ps) = case p of
  RestPattern _ -> 1 + countRestPattern ps
  _ -> countRestPattern ps

structPattern :: Parser Pattern
structPattern = do
  name <- identifier
  fields <- braces $ sepEndBy1 structFieldPattern (symbol' ",")
  return $ StructPattern name fields
  where
    structFieldPattern = try explicitFieldPattern <|> shorthandFieldPattern

    explicitFieldPattern = do
      name <- identifier
      symbol' "="
      pat <- pattern_ False
      return (name, pat)

    shorthandFieldPattern = do
      name <- identifier
      return (name, BindPattern name) -- Using the field name as a variable pattern)

bindPattern :: Parser Pattern
bindPattern = BindPattern <$> identifier

litPattern :: Parser Pattern
litPattern = LitPattern <$> literal

-- Expression parser
expr :: Parser PExpr
expr = do
  pos <- getSourcePos
  e <- makeExprParser term operatorTable
  return $ PWithPos pos e
  where
    term =
      choice
        [ try $ withPos tupleExpr,
          try $ parens expr,
          withPos arrayExpr,
          withPos litExpr,
          withPos lambdaExpr,
          try $ withPos structExpr,
          withPos varExpr
        ]

withPos :: Parser PExpr -> Parser PExpr
withPos p = do
  pos <- getSourcePos
  PWithPos pos <$> p
  
arrayExpr :: Parser PExpr
arrayExpr = PArrayExpr <$> brackets (sepBy expr comma)

tupleExpr :: Parser PExpr
tupleExpr = parens $ do
  expr' <- expr <* symbol' ","
  exprs <- expr `sepBy1` symbol' ","
  return $ PTupleExpr (expr' : exprs)

varExpr :: Parser PExpr
varExpr = PVarExpr <$> identifier

litExpr :: Parser PExpr
litExpr = PLitExpr <$> literal

lambdaExpr :: Parser PExpr
lambdaExpr = do
  symbol' "\\"
  params <- parens $ sepBy1 param (symbol ",")
  symbol' "->"
  block <- optional $ lookAhead (symbol "{")
  case block of
    Just _ -> PLambdaBlock params <$> braces (many stmt)
    Nothing -> PLambdaExpr params <$> expr
  where
    param = do
      name <- identifier
      symbol' ":"
      ty <- type_
      return (name, ty)

structExpr :: Parser PExpr
structExpr = do
  name <- identifier
  fields <- braces $ sepEndBy1 structField (symbol' ",")
  return $ PStructExpr name fields
  where
    structField = do
      name <- identifier
      symbol' "="
      expr' <- expr
      return (name, expr')

-- Statement parser
stmt :: Parser PStmt
stmt = do
  pos <- getSourcePos
  s <- choice [caseStmt, returnStmt, varDeclStmt, whileStmt, forStmt, ifStmt, try assignStmt, exprStmt]
  return $ PWithPosStmt pos s

-- Case statement parser
caseStmt :: Parser PStmt
caseStmt = do
  reserved "case"
  expr' <- expr
  branches <- braces (many caseBranch)
  return $ PCaseStmt expr' branches
  where
    caseBranch = do
      pat <- pattern_ True
      guard' <- optional (reserved "if" *> expr)
      symbol' "=>"
      body <- braces (many stmt)
      return $ PCaseBranch pat guard' body

-- Return statement parser
returnStmt :: Parser PStmt
returnStmt = do
  reserved "return"
  noExpr <- optional (symbol' ";")
  case noExpr of
    Just _ -> return $ PReturnStmt Nothing
    Nothing -> do
      expr' <- expr
      case expr' of
        PLambdaBlock _ _ -> return $ PReturnStmt (Just expr')
        _ -> do
          symbol' ";"
          return $ PReturnStmt (Just expr')

-- Assignment statement parser
assignStmt :: Parser PStmt
assignStmt = do
  lhs <- expr
  symbol' "="
  rhs <- expr
  case rhs of
    PLambdaBlock _ _ -> return $ PAssignStmt lhs rhs
    _ -> do
      symbol' ";"
      return $ PAssignStmt lhs rhs

varDeclStmt :: Parser PStmt
varDeclStmt = do
  reserved "let"
  offset <- getOffset
  pat <- varPattern True
  end <- getOffset
  ty <- optional (symbol' ":" *> type_)
  expr' <- optional (symbol' "=" *> expr)
  correctVarDecl pat ty expr' offset end
  case expr' of
    Just (PLambdaBlock _ _) -> return $ PVarStmt pat ty expr'
    _ -> do
      symbol' ";"
      return $ PVarStmt pat ty expr'

forStmt :: Parser PStmt
forStmt = do
  reserved "for"
  pat <- forPattern True
  reserved "in"
  expr' <- expr
  body <- braces (many (continueStmt <|> breakStmt <|> stmt))
  return $ PForStmt pat expr' body

ifStmt :: Parser PStmt
ifStmt = do
  reserved "if"
  cond <- expr
  thenBody <- braces (many stmt)
  elseBody <- optional (reserved "else" *> braces (many stmt))
  return $ PIfStmt cond thenBody elseBody

whileStmt :: Parser PStmt
whileStmt = do
  reserved "while"
  cond <- expr
  body <- braces (many (continueStmt <|> breakStmt <|> stmt))
  return $ PWhileStmt cond body

exprStmt :: Parser PStmt
exprStmt = PExprStmt <$> (expr <* symbol' ";")

continueStmt :: Parser PStmt
continueStmt = reserved "continue" >> symbol' ";" >> return PContinueStmt

breakStmt :: Parser PStmt
breakStmt = reserved "break" >> symbol' ";" >> return PBreakStmt

-- Declaration parsers
decl :: Parser PDecl
decl = do
  pos <- getSourcePos
  d <- choice [typeDecl, functionDecl, structDecl, globalVarDecl]
  return $ PWithPosDecl pos d

-- Global variable declaration parser
globalVarDecl :: Parser PDecl
globalVarDecl = do
  reserved "let"
  offset <- getOffset
  pat <- varPattern True
  end <- getOffset
  ty <- optional (symbol' ":" *> type_)
  expr' <- optional (symbol' "=" *> expr)
  correctVarDecl pat ty expr' offset end
  symbol' ";"
  return $ PVarDecl pat ty expr'

correctVarDecl :: Pattern -> Maybe PTy -> Maybe PExpr -> Int -> Int -> Parser ()
correctVarDecl pat@(BindPattern _) Nothing Nothing offset end = region (setErrorOffset offset) (failure $ VarHasNoTypeAndExpr (pretty pat) (end - offset))
correctVarDecl pat@(ArrayPattern _) _ Nothing offset end = region (setErrorOffset offset) (failure $ PatternHasNoExpr (patternToString pat) (pretty pat) (end - offset))
correctVarDecl pat@(TuplePattern _) _ Nothing offset end = region (setErrorOffset offset) (failure $ PatternHasNoExpr (patternToString pat) (pretty pat) (end - offset))
correctVarDecl pat@WildcardPattern _ Nothing offset end = region (setErrorOffset offset) (failure $ PatternHasNoExpr (patternToString pat) (pretty pat) (end - offset))
correctVarDecl pat@(ArrayPattern _) (Just _) _ offset end = region (setErrorOffset offset) (failure $ UnexpectedTypeInPattern (patternToString pat) (pretty pat) (end - offset))
correctVarDecl pat@(TuplePattern _) (Just _) _ offset end = region (setErrorOffset offset) (failure $ UnexpectedTypeInPattern (patternToString pat) (pretty pat) (end - offset))
correctVarDecl pat@WildcardPattern (Just _) _ offset end = region (setErrorOffset offset) (failure $ UnexpectedTypeInPattern (patternToString pat) (pretty pat) (end - offset))
correctVarDecl _ _ _ _ _ = return ()

-- Function declaration parser
functionDecl :: Parser PDecl
functionDecl = do
  reserved "fn"
  name <- identifier
  params <- parens $ sepByComma param
  retTy <- optional (symbol' "->" *> type_)
  body <- braces (many stmt)
  return $ PFunDecl name params retTy body
  where
    param = do
      name <- identifier
      symbol' ":"
      ty <- type_
      return (name, ty)

-- Type declaration parser
typeDecl :: Parser PDecl
typeDecl = do
  reserved "type"
  name <- identifier
  symbol' "="
  ty <- type_
  symbol' ";"
  return $ PTypeDecl name ty

-- Struct declaration parser
structDecl :: Parser PDecl
structDecl = do
  reserved "struct"
  name <- identifier
  fields <- braces (many $ structField <* symbol' ";")
  return $ PStructDecl name fields
  where
    structField = do
      name <- identifier
      symbol' ":"
      PStructField name <$> type_

-- Operator table
operatorTable :: [[Operator Parser PExpr]]
operatorTable =
  [ [ Postfix (do args <- parens (sepBy expr comma); return $ \e -> PCallExpr e args), -- Function calls
      Postfix (try $ do symbol' "."; idx <- lexeme L.decimal; return $ \e -> PTupleAccessExpr e idx), -- Tuple access
      Postfix (try $ do symbol' "."; name <- identifier; return $ \e -> PFieldAccessExpr e name), -- Field access
      Postfix (do idx <- brackets expr; return $ \e -> PArrayAccessExpr e idx) -- Array access
    ],
    [ Prefix (PUnaryExpr Minus <$ symbol' "-"),
      Prefix (PUnaryExpr Not <$ symbol' "!")
    ],
    [ InfixL (PBinaryExpr Mul <$ symbol' "*"),
      InfixL (PBinaryExpr Div <$ symbol' "/"),
      InfixL (PBinaryExpr Mod <$ symbol' "%")
    ],
    [InfixL (PBinaryExpr Concat <$ symbol' "++")],
    [ InfixL (PBinaryExpr Add <$ symbol' "+"),
      InfixL (PBinaryExpr Sub <$ symbol' "-")
    ],
    [ InfixL (PBinaryExpr Lte <$ symbol' "<="),
      InfixL (PBinaryExpr Lt <$ symbol' "<"),
      InfixL (PBinaryExpr Gte <$ symbol' ">="),
      InfixL (PBinaryExpr Gt <$ symbol' ">")
    ],
    [ InfixN (PBinaryExpr Eq <$ symbol' "=="),
      InfixN (PBinaryExpr Neq <$ symbol' "!=")
    ],
    [InfixL (PBinaryExpr And <$ symbol' "&&")],
    [InfixL (PBinaryExpr Or <$ symbol' "||")]
  ]

data ParseResult = ParseResult
  { pprogram :: PProgram,
    sourceLines :: V.Vector String
  }

-- Parse program
parseProgram :: Filename -> String -> Either (ParseErrorBundle String ParseError) ParseResult
parseProgram filename content = do 
  let parseResult = parse (between ws eof (PProgram <$> many decl)) filename content
  case parseResult of
    Left err -> Left err
    Right p -> Right $ ParseResult p (V.fromList $ lines content)
