module DiagramLanguage where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Vector
import Graphics.SVGFonts
import Parts

-- 特別な形状の射を示す矢印
-- monicの見本
monic = 
    let tailBar = fromOffsets [0 ^& 0.2]
        morph   = arrowFromLocatedTrail (fromOffsets [1 ^& 0] ) # translateY 0.1
    in tailBar <> morph
-- connectOutsideシリーズの鏃もmonicの尻尾にできないだろうか？
    --ArrowShaftにTrailをくっつけることができるか？
    --指定した矢印のスタイルを更新してmonicにできるのが理想

open = arrowV' (with & headStyle %~ fc white . lw 0.5) unitX 