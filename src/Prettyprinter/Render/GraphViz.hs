-- | Pretty-print GraphViz labels
module Prettyprinter.GraphViz (
    render,
    render',
) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Data.GraphViz.Attributes.Complete (Label(HtmlLabel))
import qualified Data.GraphViz.Attributes.HTML as H
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (
    Doc,
    SimpleDocStream(
        SFail,
        SEmpty,
        SChar,
        SText,
        SLine,
        SAnnPush,
        SAnnPop),
    layoutPretty, defaultLayoutOptions,
    )
import Data.Text.Prettyprint.Doc.Internal (textSpaces)
import Data.Text.Prettyprint.Doc.Render.Util.Panic (
    panicUnpairedPop, panicInputNotFullyConsumed, panicUncaughtFail
    )

-- | Render a document as a GraphViz label, using sensible defaults.
render :: Doc H.Attributes -> Label
render = HtmlLabel . H.Text . render' . layoutPretty defaultLayoutOptions

-- | Render a document stream as HTML text for GraphViz. This provides more fine-grained control than 'render'.
render' :: SimpleDocStream H.Attributes -> H.Text
render' = renderSimplyDecorated' (pure .: (renderText . concat)) mempty mempty


{- Util -}

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- | This is a very minor modification of 'renderSimplyDecorated', where the /text/ function is
-- additionally passed the current stack. Worth suggesting change upstream.
renderSimplyDecorated'
    :: Monoid out
    => ([ann] -> Text -> out) -- ^ Render plain 'Text'
    -> (ann -> out)  -- ^ How to render an annotation
    -> (ann -> out)  -- ^ How to render the removed annotation
    -> SimpleDocStream ann
    -> out
renderSimplyDecorated' text push pop = go []
  where
    go _           SFail               = panicUncaughtFail
    go []          SEmpty              = mempty
    go (_:_)       SEmpty              = panicInputNotFullyConsumed
    go stack       (SChar c rest)      = text stack (T.singleton c) <> go stack rest
    go stack       (SText _l t rest)   = text stack t <> go stack rest
    go stack       (SLine i rest)      = text stack (T.singleton '\n') <> text stack (textSpaces i) <> go stack rest
    go stack       (SAnnPush ann rest) = push ann <> go (ann : stack) rest
    go (ann:stack) (SAnnPop rest)      = pop ann <> go stack rest
    go []          SAnnPop{}           = panicUnpairedPop

-- | Helper for rendering an individual 'H.TextItem'.
renderText :: H.Attributes -> T.Text -> H.TextItem
renderText cs t
    | T.null t   = ti -- graphviz doesn't like an empty string between tags
    | otherwise  = H.Font cs [ti]
    where ti = H.Str $ TL.fromStrict t
