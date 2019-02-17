{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Error (
    BMXError (..)
  , renderBMXError
  , LexError (..)
  , ParseError (..)
  , renderParseError
  , FunctionError (..)
  , renderFunctionError
  , EvalError (..)
  , renderEvalError
  , EvalErrorT (..)
  , Breadcrumbs (..)
  , Breadcrumb (..)
  , indent
  ) where

import qualified Data.Text as T

import           BMX.Data.Position (SrcInfo (..), renderSrcInfo)

import           P

-- | An aggregate type for the various things that can go wrong in BMX.
-- Constructors are provided for casing, though most users will probably want
-- to use 'renderBMXError' to produce something human-readable.
data BMXError
  = BMXLexError !LexError
  | BMXParseError !ParseError
  | BMXEvalError !EvalError

-- | Produce a human-readable error (as 'Text') from a 'BMXError'.
renderBMXError :: BMXError -> Text
renderBMXError = \case
  BMXLexError e -> renderLexError e
  BMXParseError e -> renderParseError e
  BMXEvalError e -> renderEvalError e


-- -----------------------------------------------------------------------------
-- Lexer errors

newtype LexError = LexError { renderLexError :: Text }
  deriving (Eq, Show)


-- -----------------------------------------------------------------------------
-- Parser errors

data ParseError = ParseError !SrcInfo !Text
  deriving (Eq)

renderParseError :: ParseError -> Text
renderParseError (ParseError loc text) = T.unlines [ header, indent 1 text ]
  where
    header = case loc of
      NoInfo -> "Parse error: "
      SrcLoc _ _ -> "Parse error at " <> renderSrcInfo loc <> ": "

indent :: Int -> Text -> Text
indent n t = case fmap (pre <>) (T.lines t) of
  [x] -> x
  mor -> T.unlines (filter (not . T.null) mor)
  where
    pre = T.replicate n "  "


-- -----------------------------------------------------------------------------
-- Function errors

data FunctionError
  = Mismatch !Text !Text
  | Trailing !Int
  | EOF
  | NoParams

renderFunctionError :: FunctionError -> Text
renderFunctionError = \case
  Mismatch e a -> "Type mismatch (expected " <> e <> ", got " <> a <> ")"
  Trailing i -> "Too many arguments (" <> T.pack (show i) <> " unused)"
  EOF -> "Not enough arguments"
  NoParams -> "Not enough block parameters"


-- -----------------------------------------------------------------------------
-- Evaluation errors

data EvalError = EvalError EvalErrorT Breadcrumbs

newtype Breadcrumbs = Breadcrumbs { crumbs :: [Breadcrumb] }
  deriving (Monoid)

-- These are used for backtraces when errors occur
-- we're either in a Helper, a Partial, a Decorator, or a leaf
data Breadcrumb
  = InHelper !SrcInfo !Text
  | InPartial !SrcInfo !Text
  | InDecorator !SrcInfo !Text
  | InLeaf !SrcInfo !Text !Text

renderBreadcrumbs :: Breadcrumbs -> Text
renderBreadcrumbs (Breadcrumbs bcs) = T.unlines (fmap renderBreadcrumb bcs)

renderBreadcrumb :: Breadcrumb -> Text
renderBreadcrumb bc = case bc of
  InHelper    loc    name -> "In helper '" <> name <> "' invoked at " <> renderSrcInfo loc
  InPartial   loc    name -> "In partial '" <> name <> "' invoked at " <> renderSrcInfo loc
  InDecorator loc    name -> "In decorator '" <> name <> "' invoked at " <> renderSrcInfo loc
  InLeaf      loc ty name -> "In " <> ty <> " '" <> name <> "' at " <> renderSrcInfo loc

data EvalErrorT
  = TypeError       !SrcInfo !Text !Text -- ^ A type error, with "expected" and "actual" fields.
  | FunctionError   !SrcInfo !Text !FunctionError -- ^ Arity / type error for a helper / partial / decorator
  | NotFound        !SrcInfo !Text !Text -- ^ Failed lookup with no failover
  | Unrenderable    !SrcInfo !Text -- ^ Attempt to render an undefined, list or context.
  | Shadowing       !SrcInfo !Text !Text -- ^ Attempt to redefine something
  | DefUndef        !SrcInfo !Text -- ^ Attempt to define a variable as 'undefined' (using withVariable)
  | UserError       !SrcInfo !Text -- ^ Custom error thrown from a helper.

renderEvalError :: EvalError -> Text
renderEvalError (EvalError err bcs) = case err of
  TypeError       loc e a  -> ree loc $ "Type error (expected " <> e <> ", actually " <> a <> ")"
  NotFound        loc t v  -> ree loc $ "Invoked " <> t <> " '" <> v <> "' is not defined"
  Unrenderable    loc t    -> ree loc $ "Invalid mustache: cannot render '" <> t <> "'"
  Shadowing       loc t v  -> ree loc $ "The local definition of " <> t <> " '" <> v <> "' shadows an existing binding"
  DefUndef        loc t    -> ree loc $ "Attempt to define variable '" <> t <> "' as 'undefined' - no"
  UserError       loc t    -> ree loc $ T.unlines [ "Error thrown in user code",  indent 1 t ]
  FunctionError   loc t fe -> ree loc $ T.unlines [
      "Error applying " <> t
    , indent 1 $ renderFunctionError fe
    ]
  where
    ree loc t = T.unlines [ header loc, indent 1 t, indent 2 (renderBreadcrumbs bcs) ]
    header loc@(SrcLoc _ _) = "Rendering error at " <> renderSrcInfo loc <> ":"
    header NoInfo = "Rendering error:"
