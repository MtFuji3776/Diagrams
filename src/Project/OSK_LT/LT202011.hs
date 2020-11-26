module LT202011 where

import Parts
import DiagramLanguage
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))
import PGFSurface
import qualified Data.List as L (transpose)

type B = PGF

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


-- ======================= 構想などを整理した図式 ========================
flow_ver1 = do
    txts  <- mapM getPGFObj ["構成(ver.1)"     -- 1
                            ,"\\textcolor{red}{作図のウンチク}" 
                            ,"TIkZ,InkScape,etcの話と不満点" -- 3
                            ,"diagrams紹介" 
                            ,"diagramsでの作図の方法と戦略"] -- 5
    let trl  = fromOffsets [0.5 *^ unit_Y,0.5 *^unit_Y,0.5 *^unit_Y]
        t i  = view (ix $ i-1) txts
        txts' = map t [2,3,4,5]
        alga = path [2,3,4,5]
        g    = genDiagram trl txts' id alga
        w    = diameter unitX g
        d    = t 1 === strutY 0.1 === hrule w === strutY 0.1 === g
    return d

-- 図式のテーブルを生成する関数
table dss = 
    let maxXY :: V2 Double -> [[Diagram B]] -> [Double]
        maxXY u = map (foldr max 0 . map (diameter u))
        dss' = L.transpose dss
        ws = maxXY unitX dss' 
        hs = maxXY unitY dss 
        sizes = [[(w,h) | h <- hs] | w <- ws]
        boxin d (x,y) = d # centerXY <> uncurry rect (x,y) # lw none
        boxedDs = zipWith (zipWith boxin) dss sizes
    in vcat . map hcat $ boxedDs -- 行でくっつけてから列を積み上げる

test = do
    t1 <- mapM getPGFText　["い","ろ","は","に"]
    t2 <- mapM getPGFText ["ほ","へ","と","ち"]
    t3 <- mapM getPGFText ["り","ぬ","る","を"]
    t4 <- mapM getPGFText ["わ","か","よ","た"]
    return $ table [t1,t2,t3,t4]
-- 不具合あり。文字数を増やした場合に列の幅が揃ってくれない。

-- ワンステップ導出図の描画
onestep = do
    args <- mapM getPGFObj ["A","B","C"]
    ret  <- mapM getPGFObj ["D"]
    let above = hsep 0.2 args # centerXY
        below = mconcat ret # centerXY
        width = diameter unitX above
        line = hrule (1.1 * width) # centerXY # lw thick
        d = vsep 0.1 [above,line,below] :: Diagram B
    return d

-- これをRoseTree上の高階関数と組み合わせることで、導出図をRoseTreeから一気に作れるはず
    -- RoseTree向けのLensがないか探してみるべし
onestepDerive args ret =
    let args_ = map alignB args
        above = hsep 0.2 args_ # centerXY -- 0.2で固定するより、onestepの例のパースが保たれるようサイズに比例させるべき。
        below = ret # centerXY -- 導出図のroot
        width = diameter unitX above
        line = hrule (1.1 * width) # centerXY -- 架線の太さも図式全体のパースと合うよう工夫したいが、安直に書くと再帰関数なので線がだんだん細くなっていきそうな予感
        d = vsep 0.1 [above,line,below] :: Diagram B
    in d

heyting1 = do
    fs <- mapM getPGFObj ["z \\rightarrow x \\leq z \\rightarrow x" -- 1
                         ,"z \\land (z \\rightarrow x) \\leq x"
                         ,"z \\rightarrow x \\leq z \\rightarrow y"-- 3
                         ,"z \\land (z \\rightarrow x) \\leq y"
                         ,"x \\leq y"                              ] -- 5
    let f i = view (ix (i-1)) fs
        s1 = onestepDerive [f 1] (f 2)
        s2 = onestepDerive [s1,f 3] $ f 4
        s3 = onestepDerive [s2] $ f 5
    return s3

proofTree (Node x []) = x
proofTree (Node x ts) = onestepDerive (map proofTree ts) x

heyting1' = do
    fs <- mapM getPGFObj ["z \\rightarrow x \\leq z \\rightarrow y" -- 1
                         ,"z \\land (z \\rightarrow x) \\leq y"     
                         ,"z \\land (z \\rightarrow x) \\leq x"     -- 3
                         ,"z \\rightarrow x \\leq z \\rightarrow x" 
                         ,"x \\leq y"                              ] -- 5
    let f i = view (ix (i-1)) fs
        alga = path [1,2,3,4] + 2*5
        t = genTree 1 alga
        t' = fmap f t
    return $ proofTree t'