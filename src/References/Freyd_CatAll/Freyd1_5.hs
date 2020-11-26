module Freyd1_5 where

import Parts
import DiagramLanguage
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))
import PGFSurface

-- =============== エクスポート用 ===================
diagrams =         [compositionImage
                   ,defCoverPair
                   ,defEpicPair
                   ,coverThenEpic]

-- =====================1.5.1x
compositionImage = do
    objs <- mapM getPGFObj ["A","B","C","Im(f)","Im(g)","Im(fg)"]
    labs <- mapM getPGFLabel ["f" --1
                             ,"g" 
                             ,"\\overline{f}" --3
                             ,"\\overline{g}"
                             ,"i" --5
                             ,"\\overline{ig}"
                             ]
    txt <- getPGFText "・このようにして$Im(fg) \\subseteq Im(g)$が常に成り立つのがポイント"
    let o i = view (ix (i-1)) objs
        l i = view (ix (i-1)) labs
        trl = fromOffsets [unitX,unitX,unit_X+unit_Y,unitX,unit_Y]
        alga = path [1,4,6,5,3] + (1+4)*2 + 2*(3+5)
        update = actOpt 4 2 monic
               . actOpt 6 5 monic
               . actOpt 5 3 monic
            --    . actOpt 1 2 cover
            --    . actOpt 2 3 cover -- actOpt連打もビミョーかも。hsepとかhcatみたくできるのでは？
               . tackLabel 1 2 (l 1) True-- tackLabelも同様。tackLabelsでキーとオブジェクトのリストを渡せばこれ出してくれる関数作るか。
               . tackLabel 2 3 (l 2) True
               . tackLabel 4 2 (l 5) True
               . tackLabel 2 5 (l 4) False
               . tackLabel 1 4 (l 3) False
               . tackLabel 4 6 (l 6) False 
        d = genDiagram trl objs update alga
    return $ d # centerXY === strutY 0.1 === txt # centerXY


-- ==========1.513 ==============
defCoverPair = do
    txt <- getPGFText "Cover Pairの定義。射の像の合併が終域を覆い尽くしているイメージ。"
    -- txt' <- getPGFText "notation:"
    let objs = replicate 4 bc
        trl = fromOffsets [unitX,unitX,unit_X+unit_Y]
        alga1 = (1+3)*4
        alga2 = alga1 + (1+3)*2 + 2*4
        alga3 = alga2
        update2 = actOpt 2 4 monic
        update3 = actOpt_Twin 2 4 False (monic . over locTrail (translateX 0.05))
                . actOpt_Twin 2 4 True (over locTrail reverseLocTrail .over locTrail (translateX (-0.05)))
                . introTwin 2 4
        f = genDiagram trl objs 
        ds = [f id alga1,f update2 alga2,f update3 alga3]
        qs = [NoLine,Forall,Exists]
    dialan <- diagramLanguage qs ds
    return $ padded 0.1 $ txt === strutY 0.1 === dialan # centerXY


-- ========== 1.514 ===============


defEpicPair = do
    txt <- getPGFText "・epicの定義"
    let objs = replicate 4 bc
        trl = fromOffsets [unitX + unitX,unit_X + 0.5 *^ unit_Y,unit_Y]
        alga1 = (1+2)*3
        alga2 = alga1 + (1+2)*4
        alga3 = alga2 + 3*4
        qs = [NoLine,Forall,Only]
        ds = over (ix 0) (<> place (circle 0.01 # lw none) (unitX + 1.5 *^ unit_Y)) 
           $ map (genDiagram trl objs id) [alga1,alga2,alga3]
    padded 0.1 . (((txt # centerXY) === strutY 0.1) ===) . centerXY <$> diagramLanguage qs ds 

coverThenEpic = do
    txt <- getPGFText "イコライザを持つ圏では、coverならばepic"
    let objs = replicate 4 bc
        trl = fromOffsets [unit_Y,unit_X,2 *^ unitX]
        alga = 1 * (2+3) + path [4,2,3]
        update = tackLabelTwin 2 3 True reticle False
               . actOpt 4 2 equalizer
               . actOpt 1 2 cover
               . introTwin 2 3
               . actOpt 1 3 (mkArc (-0.15))
        d = genDiagram trl objs update alga
        g = txt === strutY 0.1 === d # centerXY
    return g

-- -- Parameterインスタンスと曲率から曲線トレイルを生成
-- mkArc_ trl n =
--     let p1 = atParam trl 0
--         p2 = atParam trl 1
--     in arcBetween p1 p2 n

-- -- moptsからLocatedTrailを取り出し、曲率nの曲線トレイルに置き換える関数
-- mkArc n mopts =
--     let trl = view locTrail mopts
--     in mopts & locTrail .~ mkArc_ trl n
--どちらもDiagramLanguage行き

