module AnalyticTableau where

import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Diagrams.Backend.SVG
import Graphics.SVGFonts
import Algebra.Graph hiding((===))
import Parts

-- genLabelTreeでは(~~)で辺を描いているため、boxedTextの境界が意味をなさない
-- 辺の端をTrace上にするための関数はあるだろうか？
example0 = genLabelTree 1 ["A","B","C","D","E","F"] $ 1*(2+3) + 2*(4+5) + 3*(6+7)

