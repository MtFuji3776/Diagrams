{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Guide_Text where

import Diagrams.Prelude
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Vector
import Diagrams.Backend.SVG
import Diagrams.TwoD.Size
import Control.Lens hiding((#),none)
import Guide_Arrows(easyRender,renderTest)
import qualified Graphics.SVGFonts as SF

pt = circle 0.1 # fc red

t1 = pt <> topLeftText  "top left"         <> rect 8 1
t2 = pt <> baselineText "baseline"         <> rect 8 1
t3 = pt <> alignedText 0.7 0.5 "(0.7,0.5)" <> rect 8 1

d1 =/= d2 = d1 === strutY 2 === d2

example = t1 =/= t2 =/= t3

text' s t = text t # fontSize (local s) <> strutY (s * 1.3)

example1 = center $
            text' 10 "Hello" # italic
        === text' 5 "there"  # bold # font "freeserif"
        === text' 3 "world"  # fc green



eff = (text "F" <> square 1) # fontSize (local 1)

example2 = hcat [eff,eff # scale 2, eff # scaleX 2 , eff # scaleY 2, eff # rotateBy (1/12)]

eff3 = (text "F" <> square 1) # fontSize (normalized 0.1)

example3 = hcat 
    [eff3,eff3 # scale 2, eff3 # scaleX 2, eff3 # scaleY 2, eff3 # rotateBy (1/12)]

-- なんか型エラーが怒るんだが。
    -- SF.lin2の戻り値がIO付きなのを考慮されてないようだ
text'' d s = SF.lin2 >>= \l2 -> return $ (strokeP $ SF.textSVG' (SF.TextOpts l2 SF.INSIDE_H SF.KERN False d d) s) 
          # lw none

example4 = do
    t1 <- text'' 5 "Hello"
    t2 <- text'' 3 "world"
    return $ t1 # fc blue ||| t2 # fc green

