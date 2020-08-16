{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (void)
import Data.GraphViz qualified as GV
import Data.GraphViz.Attributes.Complete qualified as GV
import Data.GraphViz.Attributes.HTML qualified as GVH
import Data.GraphViz.Types.Graph qualified as GV
import Data.List.Extra (replace)
import Data.Text.Prettyprint.Doc.Render.GraphViz qualified as GV
import Text.Pretty.Simple.Internal

main :: IO ()
main = do
    let lab = GV.HtmlLabel . GVH.Text . GV.render' $ mkAttr <$> layoutString defaultOutputOptionsDarkBg t0
        ns =
            zipWith
                GV.DotNode
                [1 ..]
                [ -- [GV.Label lab],
                  [GV.Label lab]
                ]
        es = []
    -- gr = GV.graphToDot GV.quickParams $ GV.mkGraph [] []
    void $ GV.runGraphviz (GV.mkGraph @Int ns es) GV.DotOutput "out.dot"
    void $ GV.runGraphviz (GV.mkGraph @Int ns es) GV.Svg "out.svg"

mkAttr :: Style -> GVH.Attribute
mkAttr (Style mc b i) = case mc of
    Nothing -> GVH.ID "hmmm" --TODO should this be taking a list (or Maybe?)
    Just c -> GVH.Color $
        GV.X11Color $ case c of
            Black -> GV.Black
            Red -> GV.Red
            Green -> GV.Green
            Yellow -> GV.Yellow
            Blue -> GV.Blue
            Magenta -> GV.Magenta
            Cyan -> GV.Cyan
            White -> GV.White

t0 :: String
-- t0 = noBrac "AST [] [Def ((3,1),(5,30)) (Id \"fact'\" \"fact'\") [Equation ((4,1),(4,13)) () [PBox ((4,7),(4,9)) () (PConstr ((4,8),(4,8)) () (Id \"Z\" \"Z\") [])] (Val ((4,13),(4,13)) () (NumInt 1)),Equation ((5,1),(5,30)) () [PBox ((5,7),(5,11)) () (PConstr ((5,8),(5,10)) () (Id \"S\" \"S\") [PVar ((5,10),(5,10)) () (Id \"m\" \"m_1\")])] (Binop ((5,30),(5,30)) () \"*\" (App ((5,15),(5,27)) () (Val ((5,15),(5,15)) () (Var () (Id \"natToInt\" \"natToInt\"))) (App ((5,25),(5,27)) () (Val ((5,25),(5,25)) () (Constr () (Id \"S\" \"S\") [])) (Val ((5,27),(5,27)) () (Var () (Id \"m\" \"m_1\"))))) (App ((5,32),(5,40)) () (Val ((5,32),(5,32)) () (Var () (Id \"fact'\"\"fact'\"))) (Val ((5,38),(5,40)) () (Promote () (Val ((5,39),(5,39)) () (Var () (Id \"m\" \"m_1\")))))))] (Forall ((3,9),(3,26)) [((Id \"n\" \"n_0\"),KPromote (TyCon (Id \"Nat\" \"Nat\")))] (FunTy (Box (CInterval {lowerBound = CNat 1, upperBound = CVar (Id \"n\" \"n_0\")}) (TyApp (TyCon (Id \"N\" \"N\")) (TyVar (Id \"n\" \"n_0\")))) (TyCon (Id \"Int\" \"Int\"))))]"
t0 = noBrac "AST [] (3,1)]"

noBrac :: String -> String
noBrac = replace "[" "(" . replace "]" ")"

-- t0 = "AA 0 1"

--TODO prettyprinter-graphviz struggles with swauare brackets and newlines
