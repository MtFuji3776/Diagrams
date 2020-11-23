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
    in padded 0.1 <$> diagramLanguage qs (ds1 ++ ds2)


-- exponential = do
--     objs <- mapM getPGFObj []
--     labs <- mapM getPGFLabel []
--     let objs = replicate 6 bc
--         trl = fromOffsets [unit_Y + unitX, unit_X, 1.5 *^ unit_X, 2.5 *^ unitX + unitY , 1.5 *^ unitY]
--         alga1 = 1 + 2
--         alga2 = 3*(1+2) + 3*4
--         alga3 = alga2 + 5*(1+2+6)
--         alga4 = 5*(1+2+3+6) + 3*(1+2+4) + 6*4
--         qs = [Forall,Exists,Forall,ExistsOnly]
--         update mp = mp & fmap (arrOpts.headLength .~ (local 0.08)) -- Lensの中にこうやってfmapを混ぜ込める
--                        & actOpt 3 1 (takeLabel_ (prd # scaleX (-1)) 0.25 0 False)
--                        & actOpt 5 6 (takeLabel_ (prd # scaleX (-1))  0.16 0 True)
--         ds = map (alignB . rotateBy (1/8)) . over (ix 0) (atop (place (circle 0.001 # lw none) (unit_Y + 1.5 *^ unit_X)))
--            $ map (genDiagram trl objs update) [alga1,alga2,alga3,alga4] -- これは抽象化できるのではないか？
--     diagramLanguage qs ds

compositionOfRelations = do
    objs <- mapM getPGFObj ["A_1" --1
                           ,"T_1"
                           ,"A_2" --3
                           ,"T_2"
                           ,"A_3" --5
                           ,"T_{12}"
                           ,"P" --7
                           ,"A_1 \\times A_3"]
    let trl = fromOffsets [unitX + unitY,unitX + unit_Y,unitX + unitY,unitX + unit_Y
                          ,2 *^ unit_X + 2 *^ unitY,unitY,unitY]
        alga1 = 2*(1+3) + 4*(3+5)
        alga2 = alga1 + 6 * (2+4)
        alga3 = alga2 + 6*8 + 8*(1+5)
        alga4 = 6 * (2+4+7)
             + 7*(2+4+8)
             + 8 * (1+5)
             + (2+4)*3
             + 2*(1+3)
             + 4*(3+5)
        update = actOpt 7 8 monic
               . actOpt 6 7 cover
        lin1 = (0.75 ^& 0.75) ~~ (1.25 ^& 0.75) -- これらシンボルもLocatedTrailから位置を計算して自動で描画させたい
        lin2 = (2.75 ^& 0.75) ~~ (3.25 ^& 0.75)
        pullback = place (plb # rotateBy (-1/8) # scale (sqrt 2)) (1.75 ^& 1.75)
        -- product = place prd (1.75 ^& 4.25)
        lin = flip atop (mconcat [lin1,lin2])
        pl  = flip atop  pullback
        ds_ =  map (genDiagram trl objs update) [alga1,alga2,alga3,alga4] -- genDiagramを改良して、矢印を描画する前にオブジェクトを図式に組み込めるようにしたい
        d i f = over (ix (i-1)) f
        ds = map (padded 0.1) . d 1 lin . d 2 (lin . pl) . d 3 (lin . pl) . d 4 (lin . pl) $ ds_
        qs = [NoLine,NoQuant,NoQuant,NoQuant]
    padded 0.1 <$> diagramLanguage qs ds

snake = do
    objs2 <- mapM getPGFObj ["0","A","0","0","0","B","0","0"]
    let objs1 = replicate 10 bc
        trl = fromOffsets [unitX,unitY,unit_X,unitY,unitX,unitY,unitX,unit_Y,unit_Y ,
                            unitX+unitY,unitY,unitX,2 *^ unit_X + unitY,3 *^ (unit_X + unit_Y),unit_Y,unit_X,2 *^ unitX + unit_Y]
        alga = path [5,4,1,18] + path [7,6,3,2] + path [14,8,9,10] + path [7,8,12,13]
             + path [5,6,9,11] + path [15,4,3,10] + path [17,16,1,2]
        update = id
    return $ padded 0.1 $ genDiagram trl (objs1 ++ objs2) update alga