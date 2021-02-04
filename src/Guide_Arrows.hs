{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Guide_Arrows() where

import Diagrams.Prelude
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Vector
import Diagrams.Backend.SVG
import Diagrams.TwoD.Size
import Control.Lens hiding((#),none)

sPt = 0.50 ^& 0.50
ePt = 5.2 ^& 0.50

ex1 = arrowBetween sPt ePt <> topLeftText "ex1"

d1 = octagon 1 # lc blue # lw ultraThick # showOrigin
ds = d1 # named "1" ||| strut 3 ||| d1 # named "2"

ex23 = ds # connect "1" "2" 
          # connectPerim "1" "2" (15/16 @@ turn) (9/16 @@ turn)

ex4 = arrowAt (0 ^& 0) unit_Y

example1 = (ex1 
           ===
           strutY 0.5
           ===
           (ex23 <> ex4)) # center

easyRender name diag = renderPretty ("/Users/fujimotomakoto/Documents/latexs/DailyStrategy/202010/img/" ++ name) fixedSize diag

renderTest = easyRender "test.svg"

setSize :: Num n => n -> n -> SizeSpec V2 n
setSize w h = mkSizeSpec2D (Just w) (Just h)

fixedSize :: Num n => SizeSpec V2 n
fixedSize = setSize 400 300


c = circle 2 # fc lightgray # lw none # showOrigin

row1 = hsep 3 $ map (flip named c) ["1","3","5","7"]
row2 = hsep 3 $ map (flip named c) ["2","4","6","8"]

d2 = row1 === strutY 5 === row2

shaft1 = trailFromVertices (map p2 [(0,0), (1,0), (1,0.2), (2,0.2)])
shaft2 = cubicSpline False (map p2 [(0,0), (1,0), (1,0.2), (2,0.2)])
shaft3 = arc xDir (1/6 @@ turn)

example2 = d2 # connect' (with & arrowTail .~ quill & lengths .~ large
                              & tailTexture .~ solid orange & headTexture .~ solid orange
                              & arrowHead .~ spike
                              & shaftStyle %~ lw veryThick) "1" "2"
             # connect' (with & arrowTail .~ thorn' & lengths .~ large
                              & arrowHead .~ thorn
                              & arrowShaft .~ shaft1 & shaftStyle %~ lw veryThick) "3" "4"
             # connect' (with & arrowTail .~ block & gaps .~ small
                              & arrowHead .~ dart & headLength .~ large
                              & arrowShaft .~ shaft2
                              & headStyle %~ fc blue & tailStyle %~ fc blue
                              & shaftStyle %~ lw veryThick . lc blue) "5" "6"
             # connect' (with & arrowShaft .~ shaft3
                              & arrowHead  .~ tri & headLength .~ large
                              & headStyle  %~ fc red . opacity 0.5
                              & shaftStyle %~ lw veryThick . lc black . opacity 0.5) "7" "8"