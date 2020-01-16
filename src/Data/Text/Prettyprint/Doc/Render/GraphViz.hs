module Data.Text.Prettyprint.Doc.Render.GraphViz (
    GraphVizRenderError(..),
    render,
    renderSDS,
) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Control.Exception (
        Exception,
        displayException,
        throw,
    )
import Data.GraphViz.Attributes.HTML (
        Attribute,
        Attributes,
        Text,
        TextItem(..),
    )
import Data.Text.Prettyprint.Doc (
        Doc,
        SimpleDocStream (..),
        defaultLayoutOptions,
        layoutPretty,
    )

-- | Render a document as a GraphViz label, using 'defaultLayoutOptions'.
render :: Doc Attribute -> Text
render = renderSDS . layoutPretty defaultLayoutOptions

-- | Render a document stream as a GraphViz label.
renderSDS :: SimpleDocStream Attribute -> Text
renderSDS =
    let go cs = \case
            SFail           -> throw GVDocStreamFail
            SEmpty          -> []
            SChar c ds      -> renderText cs (T.singleton c) : go cs ds
            SText _ txt ds  -> renderText cs txt : go cs ds
            SLine n ds      -> Newline [] : renderText cs (T.replicate n " ") : go cs ds
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
renderText :: Attributes -> T.Text -> TextItem
renderText cs = Font cs . pure . Str . TL.fromStrict
