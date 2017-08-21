{-# LANGUAGE TypeFamilies #-}
module Parse where

import qualified Timeout as T

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String


parse c
  | r <- Text.Megaparsec.parse (some stmt) "" c
  = case r of
      Left e -> error $ parseErrorPretty' c e
      Right x -> x

parseFile p = Parse.parse <$> readFile p


expr :: Parser T.Expr
expr = makeExprParser term
  [ [ Prefix $ T.Neg <$ symbol "-" ]
  , [ InfixL $ T.Eq  <$ symbol "="
    , InfixL $ T.Neq <$ symbol "!="
    , InfixL $ T.Gt  <$ symbol ">"
    , InfixL $ T.Lt  <$ symbol "<"
    , InfixL $ T.Geq <$ symbol ">="
    , InfixL $ T.Leq <$ symbol "<=" ]
  , [ InfixL $ T.Pow <$ symbol "^" ]
  , [ InfixL $ T.Mod <$ symbol "%"
    , InfixL $ T.Mul <$ symbol "*"
    , InfixL $ T.Div <$ symbol "/" ]
  , [ InfixL $ T.Add <$ symbol "+"
    , InfixL $ T.Sub <$ symbol "-" ]
  ]

term = parens expr <|> T.Var <$> identifier
                   <|> T.Num <$> integer


stmt = parens stmt <|> stmtSeq

stmtSeq = f <$> sepBy1 stmt' semi
  where f l = if length l == 1 then head l else T.Seq l

stmt' = foldl1 (<|>)
  [ haltStm, nopStm, gotoStm, labelStm
  , inStm, outStm, ifStm, setStm, sendStm ]

haltStm  = T.Halt <$ rword "HALT"
nopStm   = T.NOP  <$ rword "NOP"
gotoStm  = rword "GOTO"  >> T.Goto  <$> identifier
labelStm = rword "LABEL" >> T.Label <$> identifier
inStm    = rword "IN"    >> T.In    <$> identifier
outStm   = rword "OUT"   >> T.Out   <$> expr

ifStm = do
  rword "IF"
  cond <- expr
  rword "THEN"
  T.If cond <$> stmt

setStm = do
  rword "SET"
  name <- identifier
  rword "TO"
  T.Set name <$> expr

sendStm = do
  rword "SEND"
  targ <- identifier
  rword "TO"
  time <- expr >>= \e -> (rword "BEFORE" >> return (T.Sub (T.Num 0) e))
                   <|> (rword "AFTER"  >> return e)
  occ  <- try (expr >>= \e -> symbol ":" >> return e) <|> return (T.Num 0)

  T.Send targ time occ <$> expr

-------------

rws = words "HALT LABEL GOTO IF THEN SET TO IN OUT SEND BEFORE AFTER NOP"

rword w = lexeme (string w *> notFollowedBy alphaNumChar)
lexeme  = L.lexeme sc
symbol  = L.symbol sc
parens  = between (symbol "(") (symbol ")")
integer = fromIntegral <$> lexeme L.decimal
semi    = symbol ";"

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where lineComment  = L.skipLineComment  "--"
        blockComment = L.skipBlockComment "{-" "-}"

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
