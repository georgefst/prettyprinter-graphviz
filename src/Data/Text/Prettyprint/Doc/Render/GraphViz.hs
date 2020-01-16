module Data.Text.Prettyprint.Doc.Render.GraphViz (
    GraphVizRenderError(..),
    render,
    render',
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
render = HtmlLabel . H.Text . render' . layoutPretty defaultLayoutOptions

-- | Render a document stream as HTML text for GraphViz. This provides more fine-grained control than 'render'.
render' :: SimpleDocStream H.Attribute -> H.Text
render' =
    let go cs = \case
            SFail           -> throw GVDocStreamFail
            SEmpty          -> []
            SChar c ds      -> renderText cs (T.singleton c) : go cs ds
            SText _ txt ds  -> renderText cs txt : go cs ds
            SLine n ds      -> H.Newline [] : renderText cs (T.replicate n " ") : go cs ds
            SAnnPush col ds -> go (col : cs) ds
            SAnnPop ds      -> go (tailDef (throw GVEmptyStack) cs) ds
    in  go []

-- | Possible rendering errors - these indicate a corrupted 'SimpleDocStream'.
data GraphVizRenderError
    = GVDocStreamFail
    | GVEmptyStack
    deriving (Eq, Ord, Read, Show)
instance Exception GraphVizRenderError where
    displayException = \case
        GVDocStreamFail -> t ++ "encountered failure in document stream"
        GVEmptyStack    -> t ++ "attempted to pop empty attribute stack"
        where t = "Failed to render HTML for GraphViz: "

-- | Equal to the funciton of the same name from [safe](https://hackage.haskell.org/package/safe).
tailDef :: [a] -> [a] -> [a]
tailDef e = \case
    []     -> e
    _ : xs -> xs

-- | Helper for rendering an individual 'TextItem'.
renderText :: H.Attributes -> T.Text -> H.TextItem
renderText cs = H.Font cs . pure . H.Str . TL.fromStrict
