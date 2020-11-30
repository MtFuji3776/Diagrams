module LT202011 where

import Parts
import DiagramLanguage
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))
import PGFSurface
import qualified Data.List as L (transpose)

type B = PGF

renderLT' name w h = renderOnlinePGF' ("/Users/fujimotomakoto/Documents/latexs/DailyStrategy/work/osklt202011/" ++ name) $ luaSurafaceSize w h 

renderLT name = renderLT' name 300 225


-- スライド用の図式

samples = [logos,compositionOfRelations,snake]

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


-- ===================== TrailLikeとfromOffsetsの解説：文脈に応じてTrail,LocatedTrail,Path,[P2 Double]に解釈されるところを見せる
    -- それってワンチャン、ターミナル上の実行結果の値を見せるのでも良いのでは？
    -- ↑微妙。それより同一のオブジェクトが変換関数に応じて別々の出力を与えるのを画像で示した方がわかりやすい
trailkei = [trailLike1,trailLike2,trailLikes]

trailLike1 = do
    txt <- getPGFText "\\texttt{\\textcolor{red}{atPoints} (square 1) (replicate 4 bc)}"
    txt' <- getPGFText "\\texttt{where bc = circle 0.05 \\# fc black}"
    let d = atPoints (square 1) (replicate 4 bc)
    return . padded 0.1 $ d === strutY 0.1 === txt === txt' # translateX (0.1)

trailLike2 = do
    txt <- getPGFText "\\texttt{\\textcolor{red}{strokePath} (square 1)}"
    let d = strokePath $ square 1
    return .padded 0.1 $ d === strutY 0.1 === txt

trailLikes = do
    txt <- getPGFText "\\texttt{square 1}という一つの値が、使われる文脈に応じて別の値を取る"
    t1 <- centerXY <$> trailLike1
    t2 <- centerXY <$> trailLike2
    return . padded 0.1 $ txt === strutY 0.1 === (t1 ||| vrule 2 # centerXY . lw thin ||| t2) # centerXY


-- segments = do
--     txt <- getPGFText "\\texttt{[(1,1),(-1,1),(-1,-1),(1,-1)]}"
--     let seg = fromOffsets [unitX + unitY,unit_X + unitY,unit_X + unit_Y,unitX + unit_Y]
--     return 



-- ===================== atop,beside,Origin,Envelopeの解説用図式
atopetc = [circleAndPentagon,cpAtop,cpBeside1,cpBeside2,cpBeside3]

circleAndPentagon = do
    txt1 <- getPGFText "\\texttt{circle 1 \\# showOrigin}"
    txt2 <- getPGFText "\\texttt{pentagon 1 \\# showOrigin}"
    let c = circle 1 # showOrigin
        p = pentagon 1 # showOrigin
        ct = c === strutY 0.1 === txt1
        cp = p === strutY 0.1 === txt2
    return . padded 0.1 $ ct ||| strutX 0.5 ||| cp

cpAtop = do
    txt <- getPGFText "\\texttt{circle 1 \\# showOrigin <> pentagon 1 \\# showOrigin}"
    let c = circle 1 # showOrigin
        p = pentagon 1 # showOrigin
    return . padded 0.1 $ (c <> p) === strutY 0.1 === txt

cpBeside1 = do
    txt <- getPGFText "\\texttt{circle 1 \\# showOrigin ||| pentagon 1 \\# showOrigin}"
    let c = circle 1 # showOrigin
        p = pentagon 1 # showOrigin
    return $ padded 0.1 $ (c ||| p) # centerXY === strutY 0.1 === txt

cpBeside2 = do
    txt1 <- getPGFText "\\texttt{circle 1 \\# showOrigin === pentagon 1 \\# showOrigin}"
    txt2 <- getPGFText "\\texttt{pentagon 1 \\# showOrigin === circle 1 \\# showOrigin}"
    let c = circle 1 # showOrigin
        p = pentagon 1 # showOrigin
    return $ padded 0.1 $ ((c === p) # centerXY === strutY 0.1 === txt1) ||| strutX 0.5 ||| ((p === c) # centerXY === strutY 0.1 === txt2)

cpBeside3 = do
    txt <- getPGFText "\\texttt{beside (unitX + 0.5 *\\^\\ unitY) (circle 1 \\# showOrigin) (pentagon 1 \\# showOrigin)}"
    let c = circle 1 # showOrigin # named "1"
        p = pentagon 1 # showOrigin # named "2"
        d = centerXY $ beside (unitX + 0.5 *^ unitY) c p <> arrowV (0.8 *^ normalize $ unitX + 0.5 *^ unitY) # lc red
    return $ padded 0.1 $ d === strutY 0.1 === txt


-- ================== 実例　==================
example = [getPoints,alga,skelton,exponential]

getPoints = do
    objs <- mapM getPGFObj ["1","2","3","4","5","6"]
    txt <- getPGFText "\\texttt{rotateBy (-1/8) \\$ fromOffsets [1 \\^\\ \\& 1,0\\^\\ \\&(-1),0\\^\\ \\&(-1.5),1\\^\\ \\&2.5,1.5\\^\\ \\&0]}"
    let o i = view (ix $ i-1) objs
        trl = rotateBy (-1/8) $ fromOffsets [unitX + unitY,unit_Y,1.5 *^ unit_Y, 2.5 *^ unitY + unit_X,1.5 *^ unit_X]
        alga = path [1,2,3,4,5,6]
        d = genDiagram trl objs id alga # centerXY
        update = fmap (fc red)
    return $ lc magenta $ d === strutY 0.1 === txt

alga = do
    objs <- mapM getPGFObj ["1","2","3","4"]
    txt <- getPGFText "1*(2+3*4)"
    let o i = view (ix $ i-1) objs
        trl = fromOffsets [unitX + unitY,unit_Y,unit_Y]
        alga = 1*(2+3*4)
        d = genDiagram trl objs id alga
    return $ d # centerXY === strutY 0.1 === txt

skelton = do
    objs <- mapM getPGFObj $ map show [1,2,3,4,5,6]
    txts <- mapM getPGFObj ["\\texttt{alga1} = 1+2","\\texttt{alga2} = \\texttt{alga1} + 3*(1+2+4)","\\texttt{alga3} = \\texttt{alga2} + 5*(1+2+6)","\\texttt{alga3} + 5*3 + 6*4"]
    let trl = rotateBy (-1/8) $ fromOffsets [unitX + unitY,unit_Y,1.5 *^ unit_Y, 2.5 *^ unitY + unit_X,1.5 *^ unit_X]
        alga1 = 1 + 2
        alga2 = alga1 + 3*(1+2+4)
        alga3 = alga2 + 5*(1+2+6)
        alga4 = alga3 + 5*3 + 6*4
        f = over (ix 0) (<> place (circle 0.01 # lw none) (1 ^& (-1.5)))
        ds = zipWith (\x y -> x === strutY 0.1 === y) (map centerXY $ f $ map (genDiagram trl objs id) [alga1,alga2,alga3,alga4]) txts
        qs = [Forall,Exists,Forall,ExistsOnly]
    padded 0.1 <$> diagramLanguage qs ds


exponential = do
    objs <- mapM getPGFObj ["A","B","A \\times B^A","B^A","X \\times A","X"]
    labs <- mapM getPGFLabel ["f" --1
                            ,"ev"
                            ,"1 \\times \\lambda f" --3
                            ,"\\lambda f"]
    let trl = rotateBy (-1/8) $ fromOffsets [unitX + unitY,unit_Y,1.5 *^ unit_Y, 2.5 *^ unitY + unit_X,1.5 *^ unit_X]
        alga1 = 1 + 2
        alga2 = alga1 + 3*(1+2+4)
        alga3 = alga2 + 5*(1+2+6)
        alga4 = alga3 + 5*3 + 6*4
        l i = view (ix $ i-1) labs
        update = tackLabel 5 3 (l 3) True
               . tackLabel 3 4 (l 2) True
               . tackLabel 6 4 (l 4) False
        f = over (ix 0) (<> place (circle 0.01 # lw none) (1 ^& (-1.5)))
        ds = f $ map (genDiagram trl objs update) [alga1,alga2,alga3,alga4]
        qs = [Forall,Exists,Forall,ExistsOnly]
    padded 0.1 <$> diagramLanguage qs ds

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

-- ===================== その他 =======================

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

