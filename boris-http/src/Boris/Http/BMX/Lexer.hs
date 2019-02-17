{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module BMX.Lexer (
    LexError (..)
  , tokenise
  , validIdChar
  ) where

import           Data.Char (isSpace)
import qualified Data.Text as T
import           Text.Parsec hiding ((<|>), string, tokens, token)
import qualified Text.Parsec as Parsec

import           Text.Parsec.Text
import           Text.Read (read)

import           BMX.Data

import           P hiding (many, null)


tokenise :: Text -> Either LexError [Positioned Token]
tokenise t = bimap (LexError . T.pack . show) id (Parsec.parse tokens "" t)

tokens :: Parser [Positioned Token]
tokens = mconcat <$> many token <* eof


-- -----------------------------------------------------------------------------
-- Character classes
--

notNull :: Parser Char
notNull = satisfy (/= '\0')

-- | When one these characters follow, '.' and ".." get treated as IDs
idLookAhead :: Parser Char
idLookAhead = try $ satisfy predi
  where predi c = or [isSpace c, inClass idClass c]
        idClass = ['=', '~', '}', '/', '.', ')', '|']

literalLookAhead :: Parser Char
literalLookAhead = satisfy predi
  where predi c = or [isSpace c, inClass litClass c]
        litClass = ['~', '}', ')']

validIdChar :: Char -> Bool
validIdChar = predi
  where predi c = and [not (isSpace c), notInClass idNegClass c]
        -- | This is a horrible mismash of ranges translated from the official lexer
        idNegClass =    ['!', '"', '#', '\0']
                     <> ['%' .. ',']
                     <> ['.', '/', ';', '<', '=', '>', '@', '[', '\\', ']', '^', '`']
                     <> ['{', '|', '}', '~']


-- -----------------------------------------------------------------------------
-- Top-level
--

token :: Parser [Positioned Token]
token = mu <|> contentP

-- | Raw Web Content
contentP :: Parser [Positioned Token]
contentP = do
  body@(b :@ _) <- withPos $ try (manyTillUnescaped notNull open) <|> plain
  guard (not (T.null b))
  pure [fmap Content body]
  where
    plain = T.pack <$> many1 notNull

-- | Mustachioed blocks
mu :: Parser [Positioned Token]
mu = do
  o    <- withPos $ try open
  lf   <- fmap (o @@>) strip
  body <- blockComment lf <|> shortComment lf <|> rawBlock lf <|> unescapedMu lf <|> muExpr lf
  pure body

-- | Mustachioed expressions
muExpr :: Positioned Format -> Parser [Positioned Token]
muExpr f = do
  o <- try $ openPs f
  expr <- manyTill exprPs (lookAhead (try (strip *> close)))
  c <- end
  pure (o : expr <> [c])
  where
    end = do
      lr@(r :@ _) <- strip
      c <- withPos close
      pure (lr @@> c $> Close r)


-- | Top-level comment (block containing only a comment)
-- e.g. "{{!-- this is a top-level comment --}}"
-- Doesn't allow nested comments. "{{!-- {{!-- --}} --}}" will fail.
-- See https://github.com/wycats/handlebars.js/blob/master/src/handlebars.l#L68
blockComment :: Positioned Format -> Parser [Positioned Token]
blockComment lf@(f :@ _) = do
  o <- withPos startComment
  com <- withPos . fmap (Comment . T.pack) $ manyTill notNull (lookAhead (try endCommentBlock))
  e <- withPos endComment
  lr@(rstrip :@ _) <- strip
  c <- withPos close
  let opener = lf @@> o
      end = CloseCommentBlock rstrip <$ (e @@> lr @@> c)
  pure [opener, com, end]
  where startComment = try (string "!--") *> pure (OpenCommentBlock f)
        endComment = string "--"
        endCommentBlock = endComment *> strip *> close

-- | Top-level comment in the short format. These can't contain
-- anything mustache would parse.
-- See https://github.com/wycats/handlebars.js/blob/master/src/handlebars.l#L97
shortComment :: Positioned Format -> Parser [Positioned Token]
shortComment f = do
  _   <- try $ char '!'
  com <- withPos $ T.pack <$> manyTill notNull (lookAhead (try close))
  -- Spec has no rstrip here
  c <- withPos close
  pure [fmap OpenComment f, fmap Comment com, Close Verbatim <$ c]

-- | Values produced by these blocks are not HTML-escaped
unescapedMu :: Positioned Format -> Parser [Positioned Token]
unescapedMu lf = do
  o    <- try $ openUnescaped lf
  body <- manyTill exprPs (lookAhead (try endEscaped))
  rf   <- endEscaped
  pure (o : body <> [rf])
  where
    endEscaped :: Parser (Positioned Token)
    endEscaped = fmap (fmap CloseUnescaped) (string "}" *> strip <* close)

-- | A raw block: All content in between {{{{helper foo bar}}}} and {{{{/helper}}}} is
-- handed directly to helper without any mustache-processing.
-- Nested raw blocks are supported (ignored), but need to be balanced.
-- e.g. {{{{a}}}} {{{{this does not work}}}} {{{{/a}}}}
rawBlock :: Positioned Format -> Parser [Positioned Token]
rawBlock (Strip :@ _) = fail "raw blocks can't perform formatting"
rawBlock (Verbatim :@ _) = do
  o <- withPos (try open)
  body <- manyTill exprPs (lookAhead (close *> close))
  c1 <- withPos (close *> close)
  raw <- withPos $ RawContent . T.concat <$> many (try (nestedRaw <|> content'))
  c2 <- withPos closeRawBlock
  let openT = OpenRawBlock <$ o
      closeT = CloseRawBlock <$ c1
  pure $ openT : body <> [closeT, raw, c2]
  where
    openRaw = string "{{{{"
    closeRaw = string "}}}}"
    --
    content' = do
      a <- try $ T.pack <$> manyTill notNull (lookAhead (try (openRaw <|> closeRaw)))
      guard (not (T.null a))
      pure a
    -- Nested raw blocks are parsed as Content, but nested blocks ntb balanced
    nestedRaw :: Parser Text
    nestedRaw = do
      o <- try (openRaw <* notFollowedBy (char '/'))
      body <- manyTill notNull (lookAhead (try closeRaw))
      c <- closeRaw
      bs <- many (nestedRaw <|> content')
      (CloseRaw i) <- closeRawBlock
      pure (o <> T.pack body <> c <> T.concat bs <> "{{{{/" <> i <> "}}}}")
    --
    closeRawBlock = do
      _ <- try $ openRaw *> char '/'
      i <- manyTill notNull (lookAhead (try (skipSpace *> closeRaw)))
      _ <- skipSpace *> closeRaw
      pure (CloseRaw (T.pack i))

openPs :: Positioned Format -> Parser (Positioned Token)
openPs f = openPartial f
  <|> openPartialBlock f
  <|> openBlock f
  <|> openEndBlock f
  <|> openUnescaped f
  <|> openUnescaped' f
  <|> openInverse f
  <|> openInverseChain f
  <|> openOrdinary f
  <?> "mustache opener"

exprPs :: Parser (Positioned Token)
exprPs = skipSpace *> eps <* skipSpace
  where
    eps = numberP
      <|> stringP
      <|> boolP
      <|> openSExp
      <|> closeSExp
      <|> equals
      <|> dataSigil
      <|> undef
      <|> null
      <|> openBlockParams
      <|> closeBlockParams
      <|> idP
      <?> "literal"


-- -----------------------------------------------------------------------------
-- Handlebars expression prologues
--

openPartial :: Positioned Format -> Parser (Positioned Token)
openPartial f = do
  op <- withPos . try $ char '>'
  pure (fmap OpenPartial f <@@ op)

openPartialBlock :: Positioned Format -> Parser (Positioned Token)
openPartialBlock f = do
  opb <- withPos . try $ string "#>"
  pure (fmap OpenPartialBlock f <@@ opb)

-- | {{# - block syntax
--  {{#* - decorator block
openBlock :: Positioned Format -> Parser (Positioned Token)
openBlock lf@(f :@ _) = do
  o <- withPos opens
  pure (lf @@> o)
  where
    opens = do
      _ <- try $ char '#'
      option (OpenBlock f) (try $ char '*' *> pure (OpenDecoratorBlock f))

-- | End of a block's scope.
-- e.g. {{/
openEndBlock :: Positioned Format -> Parser (Positioned Token)
openEndBlock f = do
  oeb <- withPos . try $ char '/'
  pure (fmap OpenEndBlock f <@@ oeb)

-- | A value that should not be HTML-escaped
-- e.g. {{{body}}}
openUnescaped :: Positioned Format -> Parser (Positioned Token)
openUnescaped f = do
  ou <- withPos . try $ char '{'
  pure (fmap OpenUnescaped f <@@ ou)

-- | Alternate, undocumented format for unescaped output
-- e.g. {{&body}}
openUnescaped' :: Positioned Format -> Parser (Positioned Token)
openUnescaped' f = do
  ou <- withPos . try $ char '&'
  pure (fmap OpenUnescaped f <@@ ou)

-- | {{ - ordinary expression
--  {{* - decorator
openOrdinary :: Positioned Format -> Parser (Positioned Token)
openOrdinary lf@(f :@ _) = do
  oo <- withPos $ option (Open f) (try $ char '*' *> pure (OpenDecorator f))
  pure (lf @@> oo)

openInverse :: Positioned Format -> Parser (Positioned Token)
openInverse f = do
  oi <- withPos . try $ char '^'
  pure (fmap OpenInverse f <@@ oi)

openInverseChain :: Positioned Format -> Parser (Positioned Token)
openInverseChain f = do
  oic <- withPos . try $ (skipSpace *> string "else" <* skipSpace)
  pure (fmap OpenInverseChain f <@@ oic)


-- -----------------------------------------------------------------------------
-- Handlebars expressions
--

-- | IDs are either ".", "..", a sequence of chars satisfying validIdChar, or
-- 'segment literal notation' e.g. [10].
-- Segment literals are defined as [(\\]|[^\]])*], i.e. [.*] with escaping
idP :: Parser (Positioned Token)
idP = withPos (segLit <|> dotdot <|> try dot <|> sep <|> idP')
  where
    idTrail = lookAhead (try idLookAhead)
    --
    dotdot = ID <$> try (string "..") <* idTrail
    --
    dot = do
      c <- try (char '.')
      _ <- try idTrail
      pure (ID (T.singleton c))
    --
    idP' = do
      i <- try $ takeWhile1 validIdChar
      _ <- try idTrail
      guard (not (T.null i))
      pure (ID i)
    --
    segLit = do
      _   <- try $ char '['
      lit <- manyTillUnescaped notNull (string "]")
      _   <- char ']'
      _   <- try idTrail
      pure (SegmentID lit)

numberP :: Parser (Positioned Token)
numberP = withPos $ do
  -- FIX this is kinda terrible
  neg <- option id (try $ char '-' *> pure negate)
  int <- try (read <$> many1 digit)
  _   <- option [] suffix -- discard any fractional component
  _   <- lookAhead (try literalLookAhead)
  pure (Number (neg int))
  where suffix = try $ char '.' *> many1 digit

stringP :: Parser (Positioned Token)
stringP = withPos (doubleP <|> singleP)
  where
    doubleP = do
      _   <- try $ char '"'
      str <- manyTillUnescaped notNull (string "\"")
      _   <- char '"'
      pure (String (T.replace "\\\"" "\"" str))
    singleP = do
      _   <- try $ char '\''
      str <- manyTillUnescaped notNull (string "'")
      _   <- char '\''
      pure (String (T.replace "\\'" "'" str))

boolP :: Parser (Positioned Token)
boolP = withPos (true <|> false)
  where
    true = try $ string "true" *> pure (Boolean True)
    false = try $ string "false" *> pure (Boolean False)

sep :: Parser Token
sep = Sep <$> (try (char '.') <|> try (char '/'))

equals :: Parser (Positioned Token)
equals = withPos . try $ string "=" *> pure Equals

dataSigil :: Parser (Positioned Token)
dataSigil = withPos . try $ string "@" *> pure Data

undef :: Parser (Positioned Token)
undef = withPos . try $ string "undefined" *> pure Undefined

null :: Parser (Positioned Token)
null = withPos . try $ string "null" *> lookAhead literalLookAhead *> pure Null

openBlockParams :: Parser (Positioned Token)
openBlockParams = withPos . try $ string "as" *> many1 space *> string "|" *> pure OpenBlockParams

closeBlockParams :: Parser (Positioned Token)
closeBlockParams = withPos . try $ string "|" *> pure CloseBlockParams

openSExp :: Parser (Positioned Token)
openSExp = withPos . try $ const OpenSExp <$> char '('

closeSExp :: Parser (Positioned Token)
closeSExp = withPos . try $ const CloseSExp <$> char ')'

strip :: Parser (Positioned Format)
strip = withPos $ option Verbatim (try $ string "~" *> pure Strip)

open :: Parser Text
open = try $ string "{{"

close :: Parser Text
close = try $ string "}}"


-- -----------------------------------------------------------------------------
-- Util
--

manyTillUnescaped :: Parser Char -> Parser Text -> Parser Text
manyTillUnescaped a special = do
  str <- manyTill a (lookAhead (try special))
  let c = T.pack str
  if | T.takeEnd 2 c == "\\\\" -> pure (T.dropEnd 1 c)
     | T.takeEnd 1 c == "\\"   -> do
         o <- option T.empty special
         g <- manyTillUnescaped a special
         pure (c <> o <> g)
     | otherwise               -> pure c

skipSpace :: Parser ()
skipSpace = skipMany (try space)

-- Slow version of Attoparsec's
inClass :: [Char] -> Char -> Bool
inClass = flip elem

notInClass :: [Char] -> Char -> Bool
notInClass s = not . inClass s

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = T.pack <$> many1 (try (satisfy p))

string :: [Char] -> Parser Text
string = fmap T.pack . Parsec.string

withPos :: Parser a -> Parser (Positioned a)
withPos p = do
  start <- liftM srcInfo getPosition
  val <- p
  end <- liftM srcInfo getPosition
  return (val :@ (SrcLoc start end))

srcInfo :: SourcePos -> Position
srcInfo pos = Position (sourceLine pos) (sourceColumn pos)
