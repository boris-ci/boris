{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BMX.Data.Page (
    Page (..)
  , page
  , content
  , renderPage
  , escapePage
  , Chunk (..)
  , singleChunk
  ) where

import           Data.Char (isSpace)
import           Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html.Renderer.Text as B

import           BMX.Data.Format

import           P

-- | The type of rendered templates.
data Page
  = Formatter !Format !Format Chunk Chunk Chunk
  | Formattee Chunk
  deriving (Show, Eq)

instance Monoid Page where
  mempty = Formattee (singleChunk TS.empty)
  mappend (Formattee c1) (Formattee c2) = Formattee (c1 <> c2)
  mappend (Formattee c) (Formatter lf rf body lt rt) =
    let lhs = formatEnd lf c
        lf' = newFormat lf lhs
        lt' = lhs <> lt
    in Formatter lf' rf body lt' rt
  mappend (Formatter lf rf body lt rt) (Formattee c) =
    let rhs = formatStart rf c
        rt' = rt <> rhs
        rf' = newFormat rf rhs
    in Formatter lf rf' body lt rt'
  mappend (Formatter lf1 rf1 body1 lt1 rt1) (Formatter lf2 rf2 body2 lt2 rt2) =
    let rt1' = formatEnd lf2 rt1
        lt2' = formatStart rf1 lt2
    in Formatter lf1 rf2 (body1 <> rt1' <> lt2' <> body2) lt1 rt2

-- Chunk has an O(1) mappend that we rely on to achieve linear time
newtype Chunk = Chunk { unChunk :: DList Text }
  deriving (Monoid, Show)

-- Don't care about Chunk internals
instance Eq Chunk where
  (==) = (==) `on` renderChunkStrict

{-

FIX: most nodes need to have their lines collapsed (let's call it Inlining)

"This expands the default behavior of stripping lines that are "standalone" helpers
 (only a block helper, comment, or partial and whitespace)."

In other words, the following statement types need a special kind of 'Verbatim'
that removes all whitespace up to and including the first newline:

- Partial (though there's also a gggreat indenting feature!!)
- PartialBlock
- Block
- Inverse
- InverseChain
- InverseBlock
- RawBlock (top and tail, different from the others!!! Gets its own constructor!!!)
- CommentStmt
- DecoratorBlock

FIX: Partials need to know about their indentation level. This can probably be done
     with another variant of PageFormat and some more cases in mappend, plus some Text
     indentation magic (T.unlines $ fmap (mappend whitespace) (T.lines renderedPartial))

-}

singleChunk :: Text -> Chunk
singleChunk = Chunk . DL.singleton

renderChunkStrict :: Chunk -> Text
renderChunkStrict = TL.toStrict . TL.fromChunks . DL.toList . unChunk

page :: Format -> Format -> Text -> Page
page l r body = Formatter l r (singleChunk body) mempty mempty

content :: Text -> Page
content = Formattee . singleChunk

newFormat :: Format -> Chunk -> Format
newFormat f (Chunk ls) = case f of
  Verbatim -> Verbatim
  Strip -> if and (fmap (TS.all isSpace) ls) then Strip else Verbatim

renderPage :: Page -> Text
renderPage p = renderChunkStrict $ case p of
  Formatter _ _ body lt rt -> lt <> body <> rt
  Formattee t -> t

-- | Apply formatting to the end of a page
formatEnd :: Format -> Chunk -> Chunk
formatEnd f c = case f of
  Verbatim -> c
  Strip -> stripLeft c
  where
    stripLeft (Chunk ls) = Chunk . fst $ foldr ffun (mempty, False) ls
    ffun t (dl, True) = (DL.cons t dl, True)
    ffun t (dl, False) =
      let newT = TS.dropWhileEnd isSpace t
      in if newT == TS.empty then (dl, False) else (DL.snoc dl newT, True)

-- | Apply formatting to the start of a page
formatStart :: Format -> Chunk -> Chunk
formatStart f c = case f of
  Verbatim -> c
  Strip -> stripRight c
  where
    stripRight (Chunk ls) = Chunk . fst $ foldl' ffun (mempty, False) ls
    ffun (dl, True) t = (DL.snoc dl t, True)
    ffun (dl, False) t =
      let newT = TS.dropWhile isSpace t
      in if newT == TS.empty then (dl, False) else (DL.snoc dl newT, True)

escapePage :: Page -> Page
escapePage p@(Formattee _) = p
escapePage (Formatter lf rf body lt rt) = Formatter lf rf (escapeText body) lt rt
  where escapeText (Chunk ls) = Chunk $ fmap (TL.toStrict . B.renderHtml . B.toHtml) ls
