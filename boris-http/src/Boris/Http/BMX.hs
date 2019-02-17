{-# LANGUAGE NoImplicitPrelude #-}

-- | A templating library in the style of <http://handlebarsjs.com Handlebars.js>,
-- embedded in Haskell for static or server-side rendering.
--
-- BMX templates can be written and maintained without any Haskell
-- knowledge, while the control flow (described through 'Helper'
-- functions) can be extended or replaced by the user.

module BMX (
  -- * Usage
  -- $usage

  -- * Differences from Handlebars
  -- $whatsnew

  -- * Templates
  -- $templates
    Template
  , templateFromText
  , templateToText
  , templateFile
  , bmx

  -- * Pages
  -- $pages
  , Page
  , renderPage

  -- * Rendering a Template
  -- $rendering
  , renderTemplate
  , renderTemplateM
  , BMXState
  , defaultState

  -- * Debugging a Template
  , debugTemplateIO

  -- * Errors
  , BMXError (..)
  , renderBMXError

  -- * Providing data
  -- $values
  , BMXValue (..)
  , usingContext
  , contextFromJSON
  , contextToJSON
  , maybeNull

  -- * Partials
  -- $partials
  , Partial
  , usingPartials
  , partialFile
  , partialDir
  , partialFromTemplate

  -- * Helpers
  -- $helpers
  , Helper
  , usingHelpers

  -- * Decorators
  -- $decorators
  , Decorator
  , usingDecorators
  ) where

import           BMX.Builtin (defaultState)
import           BMX.Data
import           BMX.Debug (debugTemplateIO)
import           BMX.Eval (renderTemplate, renderTemplateM, partialFromTemplate)
import           BMX.Parser (templateFromText)
import           BMX.TH (bmx, templateFile, partialFile, partialDir)

-- $usage
--
-- To load and render 'Templates', import the @BMX@ module.
--
-- To define custom helpers, partials, or decorators, import @BMX.Function@.
--
-- All other modules are internal, and should not be depended upon.

-- $whatsnew
--
-- BMX is considerably stricter than Handlebars. A number
-- of error-prone constructs that Handlebars accepts will result in a 'BMXError':
--
-- * Any attempt to print @undefined@, @null@, a list, or a 'Context'
-- (object) will result in an error. Failed lookups will not render as
-- empty strings. Use `if` or `unless` explicitly instead.
--
-- * Shadowing is restricted. Redefining variables or partials while
-- executing is difficult.
--
-- * Heavy restrictions on mutable state. Updates are restricted to
-- the current scope.
--
-- * Partials do not inherit their parent context, unless it is
-- manually passed with @{{> partialName . }}@. Pass parameters
-- explicitly using hash syntax instead, e.g.
-- @{{> partialName id=id name=someone.name }}@.
--
-- * To Be Documented
--
-- A few Handlebars features have not been implemented:
--
-- * Option hashes for helpers are not implemented, though the syntax
-- will parse.
--
-- * Partial blocks are evaluated in the outer context, as per
-- Handlebars, but this is done eagerly; it is thus not possible to
-- override variables or provide a custom context to a partial block.
-- i.e. in @{{> \@partial-block abc def=ghi }}@, @abc@ and @def@ have
-- no effect.


-- $templates BMX templates are syntactically compatible with
-- Handlebars 4.
--
-- Use 'templateFromText' to parse a 'Template' from some 'Text',
-- pretty-print it with 'templateToText', and apply it to some data
-- with 'renderTemplate' / 'renderTemplateIO'.

-- $pages Rendering a 'Template' produces a 'Page', which is little
-- more than a 'Text' field with additional formatting
-- information.
--
-- Extract the final 'Text' artefact with 'renderPage'.

-- $rendering
--
-- Apply a 'Template' to some 'BMXState' to produce a 'Page', a
-- fully-evaluated document that is ready to print.
--
-- Build up an 'BMXState' by using 'mempty' or
-- 'defaultState' as a base, and then applying 'usingContext',
-- 'usingPartials', 'usingHelpers', and 'usingDecorators' to supply
-- custom functions and data as needed.
--
-- > myEvalState = defaultState
-- >   `usingContext` coolContext
-- >   `usingPartials` [("login", partialFromTemplate loginTemplate), ("logout", partialFromTemplate logoutTemplate)]

-- $values
--
-- To make use of a 'Template', we need to supply it with data at runtime.
--
-- A context is a set of mappings from 'Text' to 'BMXValue', i.e. local
-- variable bindings. The current context is stored in the
-- 'BMXState', and is used for all variable lookups. The initial
-- context can be built from an association list with 'usingContext'.
--
-- Values can be integers, strings, booleans, lists, null, or nested
-- contexts / namespaces. Use the constructors directly.
--
-- Note that all variable names must be unique. Shadowing will lead to
-- a rendering error.

-- $partials
--
-- A 'Partial' produces a 'Page' that another 'Template' can render
-- inline. The partial has full access to the local
-- 'BMXState' when run.
--
-- Most partials will be constructed from 'Template' values using
-- 'partialFromTemplate'. However, the type is general enough to admit
-- arbitrary Haskell functions. See 'BMX.Function.partial'.

-- $helpers
--
-- A 'Helper' comes in two varieties:
--
-- * A 'BMX.Function.helper' is a function that produces a runtime
-- 'Value'.  Regular helpers can be invoked in mustache expressions,
-- and in subexpression arguments to other helpers.
--
-- * A 'BMX.Function.blockHelper' is a function that accepts two
--   'Template' parameters (roughly equivalent to @then@ and @else@
--   branches), producing a 'Page'. Block helpers can be invoked in
--   blocks, partial blocks, and inverse blocks.
--
-- A default set of helpers is provided - 'builtinHelpers'.
--
-- See <BMX-Function.html BMX.Function> for details on implementing
-- custom helpers.

-- $decorators
--
-- A 'Decorator' is a function that can make arbitrary changes to the
-- 'BMXState'. The changes made will only affect the surrounding
-- block.  Decorators are preprocessed before their containing block
-- is rendered.
--
-- A default set of decorators is provided - 'builtinDecorators'.
--
-- See <BMX-Function.html BMX.Function> for details on implementing
-- custom decorators.
