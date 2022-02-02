-- | Pretty-print GraphViz labels
module Prettyprinter.Render.GraphViz (
    render,
    render',
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.GraphViz.Attributes.Complete (Label (HtmlLabel))
import qualified Data.GraphViz.Attributes.HTML as H
import Prettyprinter (
    Doc,
    SimpleDocStream (
        SAnnPop,
        SAnnPush,
        SChar,
        SEmpty,
        SFail,
        SLine,
        SText
    ),
    defaultLayoutOptions,
    layoutPretty,
 )
import Prettyprinter.Internal (textSpaces)
import Prettyprinter.Render.Util.Panic (
    panicInputNotFullyConsumed,
    panicUncaughtFail,
    panicUnpairedPop,
 )

-- | Render a document as a GraphViz label, using sensible defaults.
render :: Doc H.Attributes -> Label
render = HtmlLabel . H.Text . render' . layoutPretty defaultLayoutOptions

-- | Render a document stream as HTML text for GraphViz. This provides more fine-grained control than 'render'.
render' :: SimpleDocStream H.Attributes -> H.Text
render' =
    renderSimplyDecorated'
        (pure .: (renderText . concat))
        mempty
        mempty
        [H.Newline [H.Align H.HLeft]]

{- Util -}

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- note that I once tried to upstream this, and it may yet be worth it: https://github.com/quchen/prettyprinter/pull/164

{- | This is a minor modification of 'renderSimplyDecorated', where the /text/ function is
additionally passed the current stack, and newlines can be handled specially.
-}
renderSimplyDecorated' ::
    Monoid out =>
    -- | Render plain 'Text'
    ([ann] -> T.Text -> out) ->
    -- | How to render an annotation
    (ann -> out) ->
    -- | How to render the removed annotation
    (ann -> out) ->
    -- | How to render a newline
    out ->
    SimpleDocStream ann ->
    out
renderSimplyDecorated' text push pop newline = go []
  where
    go _ SFail = panicUncaughtFail
    go [] SEmpty = mempty
    go (_ : _) SEmpty = panicInputNotFullyConsumed
    go stack (SChar c rest) = text stack (T.singleton c) <> go stack rest
    go stack (SText _l t rest) = text stack t <> go stack rest
    go stack (SLine i rest) = newline <> text stack (textSpaces i) <> go stack rest
    go stack (SAnnPush ann rest) = push ann <> go (ann : stack) rest
    go (ann : stack) (SAnnPop rest) = pop ann <> go stack rest
    go [] SAnnPop{} = panicUnpairedPop

-- | Helper for rendering an individual 'H.TextItem'.
renderText :: H.Attributes -> T.Text -> H.TextItem
renderText cs t
    | T.null t = ti -- graphviz doesn't like an empty string between tags
    | otherwise = H.Font cs [ti]
  where
    ti = H.Str $ TL.fromStrict t
