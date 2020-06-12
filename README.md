GraphViz pretty-printing
===================================

Just some simple utility functions for hooking up the Haskell libraries [graphviz](https://hackage.haskell.org/package/graphviz) and [prettyprinter](https://hackage.haskell.org/package/prettyprinter).

Given a [Doc](https://hackage.haskell.org/package/prettyprinter/docs/Data-Text-Prettyprint-Doc.html#t:Doc), you can use [render](https://hackage.haskell.org/package/prettyprinter-graphviz/docs/Data-Text-Prettyprint-Doc-Render-GraphViz.html#v:render) to transform it to a GraphViz [Label](https://hackage.haskell.org/package/graphviz/docs/Data-GraphViz-Attributes-Complete.html#t:Label). If you are using a different annotation type (eg. something more abstract), then you can define a mapping to GraphViz HTML attributes, and `fmap` it over your `Doc`.

![Example](https://raw.github.com/georgefst/prettyprinter-graphviz/master/example.svg)
