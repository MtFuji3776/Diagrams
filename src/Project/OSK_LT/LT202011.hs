module LT202011 where

import Parts
import DiagramLanguage
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))
import PGFSurface

-- スライド用の図式

logos = 
    let objs = replicate 7 bc
        trl = fromOffsets [unitY, unitY, unitY,unitX,unit_Y,2 *^ unit_Y]
        alga1 = path [2,1,7]
        alga2 = alga1 + 4*(2+5) + 5*7
        alga3 = alga4
        alga4 = 4 * (3+5) 
             + 3 * (2+6)
             + path [2,1,7]
             + 6 * 7
             + 5 * 6
        update1 = actOpt 2 1 monic
                . actOpt 5 6 monic
                . actOpt 6 7 monic
        update2 = actOpt 2 1 monic
               . actOpt_Twin 5 6 False monic
               . actOpt 6 7 monic
               . actOpt_Twin 5 6 True (over locTrail reverseLocTrail)
               . introTwin 5 6 
        pullback1 = place plb (0 ^& 2.75)
        pullback2 = place plb (0 ^& 1.75)
        ds1 = map (genDiagram trl objs update1) 
                            [alga1,alga2,alga3]  & (ix 1) %~ (<>pullback1)
                                                 & (ix 2) %~ (<>pullback1)
        ds2 = [genDiagram trl objs update2 alga4 <> pullback1 <> pullback2]
        qs = [Forall,Exists,Forall,Exists]
    in pad 1.1 <$> diagramLanguage qs (ds1 ++ ds2)