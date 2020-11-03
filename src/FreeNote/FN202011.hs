module FreeNote.FN202011 where

import Parts
import DiagramLanguage
import Diagrams.Prelude
import qualified Algebra.Graph as Alga hiding ((===))
import Data.Maybe (fromMaybe)
import Diagrams.TwoD.Arrow
import Diagrams.BoundingBox
import qualified Control.Lens as Lens (at,(?~))
import qualified Data.Map as Map


-- 引き戻しの図式を回転させてみた
    -- あとgenDiagramの試運転
dia1_1 =
    let rot = rotateBy (-1/8)
        loctrl = rot $ fromOffsets [unitX,unitY,unit_X,0.5*^(unit_X + unitY)]
        labels = map (flip boxedText 0.15) ["A","C","B","P","X"]
        alga   = 5*(1+4+3) + 4*(1+3) + (1+3)*2
        plbSymbol = plb # translateY 0.75 # rot
    in genDiagram loctrl labels alga <> plbSymbol


-- MorphOptsをLensで編集する練習
dia2_1 =
    let loctrl = fromOffsets [unitX + 0.5 *^ unitY] :: Trail V2 Double
        lab = attachLabel (loctrl `at` origin) (boxedText "f" 0.15 :: Diagram B) 0.5
        mopts = (def :: MorphOpts) & set locTrail ((monicShaft  loctrl) `at` origin)
                                   & set arrOpts openHead
                                   & over symbols (lab <|)
        evalMopts opts = 
            let trl = view locTrail opts
                arropts = view arrOpts opts
                symbs = view symbols opts
            in arrowFromLocatedTrail' arropts trl <> mconcat symbs
    in evalMopts mopts <> square 1 # lw none # scale 0.01

-- 良い出来なので公式採用。DiagramLanguageに移行（コピペ）
-- evalMorphOpts opts =
--     let trl     = opts ^. locTrail
--         arropts = opts ^. arrOpts
--         symbs   = opts ^. symbols
--     in arrowFromLocatedTrail' arropts trl <> mconcat symbs

_monic (Morph loctrl opts symbs) = 
    let p1 = atParam loctrl 0 
    in over locTrail (flip at p1 . monicShaft . unLoc) (Morph loctrl opts symbs)

-- LocatedTrailを一旦Trailに戻してから、道の結合演算などを施して、再びLocatedTrailに戻す
    -- 戻り値の始点は変更しない仕様
    -- 矢印のShaftを変形させるときに使うと良さそう（monic以外にarc系でも頻繁に使うはず）
-- buildLocTrail someFuncOnTrail loctrl =
--     let p0 = atParam loctrl 0
--     in flip at p0 . someFuncOnTrail . unLoc $ loctrl

dia3_1 =
    let mopts = def & locTrail .~ fromOffsets [unitX]
        trl = mopts ^. locTrail
        deco mopts = mopts & locTrail %~ buildLocTrail monicShaft
                           & symbols %~ ((attachLabel trl (boxedText "X" 0.15) 0.5 :) . (attachLabel trl reticle 0.3 :))
    in evalMorphOpts $ deco mopts

dia3_2 =
    let loctrl = fromOffsets [unitY,unitY,unitY,unit_Y + 4 *^ unitX , 2*^ unit_Y]
        objs   = map (lw none . flip box 0.05) 
                $ map (flip boxedText 0.15) 
                    [
                        "[((Int,Int),MorphOpts)]",
                        "Alga",
                        "[QDiagram]",
                        "LocatedTrail"
                    ] 
                    ++
                    [ 
                        (stroke $ textSVG "(QDiagram,Map (Int,Int) MorphOpts)" 0.30) # lw none # fc red
                    ] 
                    ++
                    [
                        boxedText "QDiagram" 0.15
                    ]
        alga = (1+2+3+4)*5 + 5*6
    in genDiagram loctrl objs alga