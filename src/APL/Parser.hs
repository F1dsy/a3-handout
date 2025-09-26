module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

-- Atom ::= var
--        | int
--        | bool
--        | "(" Exp ")"

-- FExp ::= Atom
--        | FExp FExp

-- Exp3' ::=            (* empty *)
--        | "**" Exp0

-- Exp3 ::= Exp3'

-- Exp2' ::=            (* empty *)
--        | "*" Atom Exp2'
--        | "/" Atom Exp2'

-- Exp2 ::= Exp3 Exp2'

-- Exp1' ::=            (* empty *)
--        | "+" Exp2 Exp1'
--        | "-" Exp2 Exp1'

-- Exp1 ::= Exp2 Exp1'

-- Exp0' ::=            (* empty *)
--        | "==" Exp1 Exp0'

-- Exp0 ::= Exp1 Exp0'

-- Exp  ::= Exp0
--        | “print” string Atom
--        | “get” Atom
--        | “put” Atom Atom
--
-- LExp ::= FExp
--        | "if" Exp "then" Exp "else" Exp
--        |
--

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "print",
    "get",
    "put",
    "try",
    "catch",
    "let",
    "in",
    "loop",
    "for",
    "do"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool =
  choice
    [ True <$ lKeyword "true",
      False <$ lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pFExp :: Parser Exp
pFExp = pAtom >>= chain
  where
    chain f =
      choice
        [ do
            x' <- pAtom
            chain $ Apply f x',
          pure f
        ]

-- We have done an extensive amount of work,
-- to make the following as concise as possible
pLExp :: Parser Exp
pLExp =
  choice
    [ pFExp,
      If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      Lambda
        <$> (lString "\\" *> lVName)
        <*> (lString "->" *> pExp),
      TryCatch
        <$> (lKeyword "try" *> pExp)
        <*> (lKeyword "catch" *> pExp),
      Let
        <$> (lKeyword "let" *> lVName)
        <*> (lKeyword "=" *> pExp)
        <*> (lKeyword "in" *> pExp),
      ForLoop
        <$> ((,) <$> (lKeyword "loop" *> lVName <* lString "=") <*> pExp)
        <*> ((,) <$> (lKeyword "for" *> lVName <* lString "<") <*> pExp)
        <*> (lKeyword "do" *> pExp)
    ]

pExp4 :: Parser Exp
pExp4 =
  choice
    [ pLExp,
      do
        lKeyword "print"
        str <- lString "\"" *> many (satisfy isAlphaNum) <* lString "\""
        Print str <$> pAtom,
      do
        lKeyword "get"
        KvGet <$> pAtom,
      do
        lKeyword "put"
        KvPut <$> pAtom <*> pAtom
    ]

pExp3 :: Parser Exp
pExp3 = pExp4 >>= pExp3'
  where
    pExp3' base =
      choice
        [ do
            lString "**"
            Pow base <$> pExp3,
          pure base
        ]

pExp2 :: Parser Exp
pExp2 = pExp3 >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp3
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp3
            chain $ Div x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp2
            chain $ Add x y,
          do
            lString "-"
            y <- pExp2
            chain $ Sub x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp1
            chain $ Eql x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
