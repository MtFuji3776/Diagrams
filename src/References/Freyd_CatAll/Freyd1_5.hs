module References.Freyd_CatAll.Freyd1_5 where

import Parts
import DiagramLanguage
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))
import PGFSurface

-- =============== エクスポート用 ===================
diagrams =         [("compositionImage.pdf",compositionImage)
                   ,("defCoverPair.pdf",defCoverPair)
                   ,("defEpicPair.pdf",defEpicPair)
                   ,("coverThenEpic.pdf",coverThenEpic)
                   ,("monicCoverthenEpic.pdf",monicCoverthenIsom)
                   ,("regEpicIsCover.pdf",regEpicIsCover)
                   ]

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


-- =================1.52 ================

monicCoverthenIsom = do
    objs <- mapM getPGFObj ["A","A","B"]
    labs <- mapM getPGFLabel ["1_A","f"]
    let o i = view (ix (i-1)) objs
        l i = view (ix (i-1)) labs
        trl = fromOffsets [unit_Y, unit_X]
        alga1 = 1*(2+3) + 3*2
        alga2 = alga1
        update1 = tackLabel 1 3 (l 1) False
                . tackLabel 3 2 (l 2) True
                . tackLabel 1 2 (l 2) True
                . actOpt 3 2 (monic . cover)
                . actOpt 1 2 (cover.monic)
        update2 = tackLabel 1 3 (l 1) False
                . tackLabel 1 2 (l 2) True
                . tackLabelTwin 3 2 True (l 2) True
                . actOpt_Twin 3 2 True (cover.monic)
                . actOpt 1 2 (cover.monic)
                . actOpt_Twin 3 2 False (over locTrail reverseLocTrail) 
                . introTwin 3 2
        ds = [genDiagram trl objs update1 alga1,genDiagram trl objs update2 alga2]
        qs = [NoLine,Exists]
    diagramLanguage qs ds


regEpicIsCover = do
    labs <- mapM getPGFLabel ["x" -- 1
                             ,"y"
                             ,"f" -- 3
                             ,"\\varphi"
                             ,"\\overline{f}i" -- 5
                             ,"1_{\\varphi\\Box}"
                             ,"\\overline{f}" -- 7
                             ,"i"
                             ]
    txt <- getPGFText "次の二つの図式と余イコライザの普遍性から$\\overline{f}i = 1_{\\varphi\\Box}$が成立する："
    txt1 <- getPGFText "・余イコライザの余射影はcoverであること"
    let l i = view (ix (i-1)) labs
        trl = fromOffsets [unitX,unitX,unit_Y]
        objs = replicate 4 bc
        alga = path [1,2,3,4] + 2*4
        update_ = tackLabelTwin 1 2 True (l 1) True . tackLabelTwin 1 2 True (reticle # translateY 0.05) False
                . tackLabelTwin 1 2 False (l 2) False
                . tackLabel 2 3 (l 4) True
                . tackLabel 2 4 (l 4) False
                . introTwin 1 2
        update1 = tackLabel 3 4 (l 5) True . update_ 
        update2 = tackLabel 3 4 (l 6) True . update_
        ds = map (\x -> genDiagram trl objs x alga) [update1,update2]
        d  = hsep 0.5 ds
        update3 = tackLabelTwin 3 4 True (l 7) True
                . tackLabelTwin 3 4 False (l 8) True
                . actOpt_Twin 3 4 False (monic)
                . tackLabelTwin 3 4 True (reticle # translateX 0.05) False
                . actOpt_Twin 3 4 False (over locTrail reverseLocTrail) 
                . tackLabel 2 4 (l 3) False
                . actOpt 2 4 (over symbols (\_ -> []))
                . actOpt 2 4 (mkArc $ -0.15)
                . introTwin 3 4 
                . update_
        d0 = genDiagram trl objs update3 alga
    return . padded 0.1 $ txt1 === strutY 0.1 === d0 # centerXY === strutY 0.1 === txt === strutY 0.1 === d # centerXY

pullbackInLH = do
    objs <- mapM getPGFObj ["P","\\to","Z" ,"(x,y)" ,"\\mapsto", "z = f(x) = g(y)" -- 1,2,3,4,5,6
                           ,"G_n" ,"\\stackrel{\\sim}{\\to}" ,"U_{zf}\\cap U_{zg}" ,"h = f|_{(U_{zf} \\cap U_{zg})} (g|_{U_{zf}\\cap U_{zg}})^{-1}"  --7,8,9
                           ,"f^{-1}(U_{zf} \\cap U_{zg})", "U_{zf} \\cap U_{zg}"  -- 10,11
                           ,"g^{-1}(U_{zf} \\cap U_{zg})"] -- 12
    labs <- mapM getPGFObj ["f|","g|","h = f|(g|)^{-1}"]
    txt <- getPGFText "ただし"
    let o i = view (ix $ i -1) objs
        l i = view (ix $ i - 1) labs
        alga1 = 1 * 2
        alga2 = 1*2*3
        trl1 = fromOffsets [unitX]
        trl2 = fromOffsets $ map (2 *^ ) [unit_Y,unitX + 0.5 *^ unitY]
        update2 = tackLabel 1 3 (l 1) True . tackLabel_ 1 2 (l 3) False 0.5 0.45 . tackLabel 2 3 (l 2) False . actOpt 2 3 isom . actOpt 1 3 isom
        unittrl = origin ~~ unitX
        d1 = vsep 0.05 [o 1,o 4,o 7]
        d2 = vsep 0.05 $ map (lw none . flip Parts.box 0.02 . o) [2,5,8]
        d3 = vsep 0.05 $ map o [3,6,9]
        d' = hcat [d1,d2,d3]
        d4 = hsep 0.1 [txt # centerXY,o 10 # centerXY]
        d5 = genDiagram trl2 (map o [11,12,13]) update2 alga2
        d = vsep 0.05 [d',d4] === strutY 0.1 === d5
    return d
