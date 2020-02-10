module Data.Text.Prettyprint.Doc.Render.GraphViz (
    -- * Rendering functions
    render,
    render',

    -- * Error handling
    -- The functions in this module can throw errors, given a malformed document stream.
    -- The average user is very unlikely to run into this,
    -- but error handling functionality is provided for completeness.
    GraphVizRenderError(..),
    renderSafe,
    renderSafe',
) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Control.Exception (
    Exception,
    displayException,
    throw,
    )
import Data.GraphViz.Attributes.Complete (
    Label (HtmlLabel),
    )
import qualified Data.GraphViz.Attributes.HTML as H
import Data.Text.Prettyprint.Doc (
        Doc,
        SimpleDocStream (..),
        defaultLayoutOptions,
        layoutPretty,
    )

-- | Render a document as a GraphViz label, using 'defaultLayoutOptions'.
render :: Doc H.Attribute -> Label
render = throwLeft . renderSafe

-- | Render a document stream as HTML text for GraphViz. This provides more fine-grained control than 'render'.
render' :: SimpleDocStream H.Attribute -> H.Text
render' = throwLeft . renderSafe'

data GraphVizRenderError
    = GVDocStreamFail
    | GVEmptyStack
    deriving (Eq, Ord, Read, Show)
instance Exception GraphVizRenderError where
    displayException = \case
        GVDocStreamFail -> t ++ "encountered failure in document stream"
        GVEmptyStack    -> t ++ "attempted to pop empty attribute stack"
        where t = "Failed to render HTML for GraphViz: "

-- | A total version of 'render'.
renderSafe :: Doc H.Attribute -> Either GraphVizRenderError Label
renderSafe = fmap (HtmlLabel . H.Text) . renderSafe' . layoutPretty defaultLayoutOptions

-- | A total version of 'render''.
-- This can be seen as a generalisation of any of the other functions exported by this module.
renderSafe' :: SimpleDocStream H.Attribute -> Either GraphVizRenderError H.Text
renderSafe' =
    let go cs = \case
            SFail           -> Left GVDocStreamFail
            SEmpty          -> Right []
            SChar c ds      -> renderText cs (T.singleton c) ?: go cs ds
            SText _ txt ds  -> renderText cs txt ?: go cs ds
            SLine n ds      -> H.Newline [] ?: renderText cs (T.replicate n " ") ?: go cs ds
            SAnnPush col ds -> go (col : cs) ds
            SAnnPop ds      -> flip go ds =<< maybeToEither GVEmptyStack (tailMay cs)
        infixr 0 ?:
        (?:) = fmap . (:)
    in  go []


{- Internal utilities -}

-- | On encountering a 'Left', throw it as an exception.
throwLeft :: Exception e => Either e a -> a
throwLeft = either throw id

-- | Equal to the function of the same name from [extra](https://hackage.haskell.org/package/extra).
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right

-- | Equal to the function of the same name from [safe](https://hackage.haskell.org/package/safe).
tailMay :: [a] -> Maybe [a]
tailMay = \case
    []     -> Nothing
    _ : xs -> Just xs

-- | Helper for rendering an individual 'H.TextItem'.
renderText :: H.Attributes -> T.Text -> H.TextItem
renderText cs = H.Font cs . pure . H.Str . TL.fromStrict
