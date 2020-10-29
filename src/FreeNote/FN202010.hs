module FreeNote.FN202010 where

import Parts 
import DiagramLanguage
import qualified Algebra.Graph as Alga hiding((===))
import Diagrams.Prelude
import Data.Maybe (fromMaybe)
import Diagrams.TwoD.Arrow
import Diagrams.BoundingBox
import Data.List.Split
import Diagrams.Core.Points



-- ArrowOptsを先に生成してからconnectOutside'などに渡す仕組みのフローチャートの原案
dia18_1 :: (Show n, Typeable n, RealFloat n) => QDiagram SVG V2 n Any
dia18_1 = 
    ((boxedText "Alga" 1) # named "1" 
    ||| strutX 1 
    ||| (boxedText "Map protoOpts" 1 === boxedText "キー：(Int,Int)" 0.8) # named "2"
    ||| strutX 1
    ||| ((boxedText "Map ArrowOpts" 1 === boxedText "キー：Morph Int" 0.8) # named "3"
    === strutY 2 
    === (boxedText "QDiagram" 1) # named "4"))
    # connectOutside "1" "2" . connectOutside "2" "3" . connectOutside "3" "4"
    <> place (boxedText "辺情報" 0.5) (2^&1) 
    <> place (boxedText "g:Alga" 0.5) (19 ^& (-2)) 
    <> place (boxedText "fmap" 0.5) (12.5 ^& 1)
    
-- Arrowのコンストラクタ関数の従属関係の木
    -- starを使おうとしたらDiagrams.Preludeと衝突したんだが。
dia18_2 = genBCTree 1 $ Alga.path [1,2,3] + 2*4 + Alga.star 3 [5,6,7,8]

-- dia18_3 :: Diagram B
dia18_3 = genLabelTree' (with & slWidth .~ fromMaybe (0,0) . extentX) 1 f (Alga.path [1,2,3,5] + Alga.star 3 [6,7,8] + 2*4)
    where
        f n = 
            case n of 1 -> g "arrow'"
                      2 -> g "arrowAt"
                      3 -> g "arrowBetween'"
                      4 -> g "arrowV'"
                      5 -> g "connect'"
                      6 -> g "connectPerim'"
                      7 -> g "connectOutside'"
                      8 -> g "arrowFromLocatedTrail'"
                      _ -> g ""
        g xs = boxedText xs 0.2
        -- ls =     [
        --             "arrow'"
        --            ,"arrowAt'"
        --            ,"arrowBetween'","arrowV'"
        --            ,"connect'","connectPerim'","connectOutside'","arrowFromLocatedTrail'"
        --          ]

-- ER図：多重度の表現
dia18_4 = 
    let g1 = (rect 1 0.5 # named "1" ||| (strutX 1) ||| (rect 1 0.5 # named "2"))
        g2 = (rect 1 0.5 # named "3") ||| strutX 1 ||| rect 1 0.5 # named "4"
        g3 = rect 1 0.5 # named "5" ||| strutX  1 ||| rect  1 0.5 # named "6"
        g  =  vsep 0.4 [g1 , g2 , g3]
        nohead = with & arrowHead .~ noHead
    in g # connectOutside' nohead "1" "2" # connectOutside "3" "4" # connectOutside' (with & arrowTail .~ dart') "5" "6" # pad 1.2

-- ドリル関手編の最初の問いの図１
dia20_1 = genBCDia (fromOffsets [1 ^& 0, 1^& 0]) (Alga.path[1,2,3])
       <> place (boxedText "A1" 0.1) (0.02 ^& 0.1)
       <> place (boxedText "A2" 0.1) (1.02 ^& 0.1)
       <> place (boxedText "A3" 0.1) (2.02 ^& 0.1)
       <> place (boxedText "f1" 0.1) (0.52 ^& 0.1)
       <> place (boxedText "f2" 0.1) (1.52 ^& 0.1)

-- ドリル関手編の最初の問いの図２の原型
dia20_2 = genBCDia (fromOffsets [(-1)^&1, 1^&0,1^&(-1)]) (Alga.star 2 [3,1] + 3*(1+4) + 4*1)

-- レティクル記号。本当は17日には作っていた。
dia20_3 =  mconcat [(0 ^& 0) ~~ (0.05 ^& 0),(0.15 ^& 0) ~~ (0.2 ^& 0)] # translateX (-0.1) 
        <> mconcat [(0 ^& 0) ~~ (0 ^& 0.05),(0 ^& 0.15) ~~ (0 ^& 0.2)] # translateY (-0.1)

--  ドリル関手編の最初の問いの図２にレティクル記号を追加（矢印記号をずらす方法を確立せよ）
dia20_4 = dia20_2 <> place dia20_3 (-(0.5)^&1.02) -- 矢印を平行にずらしてないので、モロ被りしないようレティクルマークをわずかにずらしている

-- 同型射を表す〜記号
    -- とりあえずながさ1のArrowに合うサイズで作ってみた
    -- scale変更しても違和感なし。
    -- Arrowのサイズにアジャストして合成するためのプログラムを作りたいかも
dia21_1 = cubicSpline False [0^&0,1^&0.2, 2^&0, 3^&(-0.2) , 4^&0]
        # scale (1/8)
        # translateX 0.25 
        # translateY 0.05

-- 同型射のサンプル。X成分の正規ベクトル。
dia21_2 = dia21_1 <> arrowV unitX

dia21_3 = square 1 # named "1" ||| circle 1 # named "2"

-- Computer ModernのotfフォントをFontForgeでSVG化。SVGフォントでAMSMath記号が使える！
    -- マジでフォントメトリックのプログラム組んでdiagramsで数式記号書き放題の機運が高まりつつある
    -- 特別な技術より地道な作業が必要になりそうなので、必要な機能から始めて地道に積み上げていくか。
dia21_4 = do
    lw <- loadFont "eufm9.svg"
    let opt = TextOpts lw INSIDE_H KERN False 0.15 0.15
    return $ textSVG_ opt "ABC" # fc black

-- 引き戻しの図式をできる限り簡単に作った
dia22_1 =
    let g = genBCDia (fromOffsets [unitX,unitY,unit_X,unit_X+unitY]) $ 5*(1+4+3) + 4*(1+3) + (3+1)*2
        plb = fromOffsets [unitX,unitY] # scale (1/4) # translateY 0.75
    in g <> plb

-- 積の図式をできる限り簡単に
    -- sqrtが関わる部分だけちょっと煩わしい。
dia22_2 =
    let g = genBCDia (fromOffsets [unitY,(1/sqrt 2) *^ (unit_X + unitY), (1/sqrt 2)*^ (unitX + unit_Y) + unitX]) (2*(1+4) + 3*(1+2+4))
        prd = fromOffsets [2*^unitX,unit_X,unitY,2*^ unit_Y] # scale 0.25 # translateY 0.75
    in rotate (-45 @@ deg) $ g <> prd

-- ラベルと黒円の対象が混合した図式を試作
    -- ghciでは問題なくコンパイルできたのにこっちではエラーが起こるのなぜ？
-- dia22_3 =
--     let g1 = genLabelDia (fromVertices [unitY] :: [P2 Double]) ["X"] 4 :: Diagram B
--         g2 = genBCDia (fromOffsets [unitX,unitY] :: [P2 Double]) $ (1+3)*2
--     in (g1 <> g2) # connectOutside 3 1 # connectOutside 1 3


-- rayTracePの実験
dia24_1 =
    let d1 = place (circle 1) (5^&0.5) :: Diagram B
        p1 = fromMaybe origin $ rayTraceP origin unitX d1
        rc = place (circle 0.05 # fc red # lw none ) p1
        arr1 = arrowAt origin unitX # lc red
    in mconcat [rc,arr1,d1]

-- subdiagramから座標を取得して利用する練習
    -- genBCDiaを使った練習
    -- うまくいったのでついでにatParamの練習も
    -- 射にラベルをつけるためのお手本になりそう
dia24_2 = 
    let g1 = genBCDia (fromOffsets [unitX,unitY]) $ ((1+3)*2 :: Alga.Graph Int) :: Diagram B
        search n = fromMaybe (mkSubdiagram mempty) $ lookupName n g1
        s1 = search (1::Int)
        s2 = search (3::Int)
        p1 = location s1
        p2 = location s2
        l_red  = p1 ~~ p2 :: Trail V2 Double
        v = atParam l_red 0.5 
        p3 = P v 
        c_blue = place (circle 0.1 # fc blue # lw none) p3 
    in  g1 <> strokeTrail l_red # lc red <> c_blue

-- 冗長さを排除
dia24_2' =
    let g1 = genBCDia (fromOffsets [unitX,unitY]) $ (1+3)*2 :: Diagram B
        getCoor n = location . fromMaybe (mkSubdiagram mempty) $ lookupName n g1
        p1 = getCoor (1::Int)
        p2 = getCoor (3::Int)
        l_red = p1 ~~ p2 :: Trail V2 Double
        p3 = P $ atParam l_red 0.5
        c_blue = place (circle 0.1 # fc blue # lw none) p3
        ln = strokeTrail l_red # lc red
    in g1 <> ln 

-- Traceにラベルを付ける方法の模索
dia24_3 =
    let g1 = genBCDia (fromOffsets [unitX,unitY]) $ (1+3)*2 :: Diagram B
        getCoor n = location . fromMaybe (mkSubdiagram mempty) $ lookupName n g1
        p1 = getCoor (1::Int)
        p2 = getCoor (3::Int)
        l_red = p1 ~~ p2 :: Trail V2 Double
        p3 = P $ atParam l_red 0.5
        c_blue = place (circle 0.05 # fc blue # lw none) p3 # withEnvelope (circle 0.07 :: Diagram B) # showEnvelope
        p4             = p3 .+^ normalize (perp $ p2 .-. p1) ^* 0.1
        circleAndLabel = c_blue <> place (boxedText "f" 0.1) p4 # withEnvelope (circle 0.08 :: Diagram B) # showEnvelope
        ln = strokeTrail l_red # lc red # withEnvelope (circle 0.06 :: Diagram B) # showEnvelope
        ln1 = strokeLine (p3 ~~ p4) # lc green
    in  ln1  <> g1 <> ln <> circleAndLabel

-- TrailとatParamの検証
dia24_4 =
    let t1 = fromOffsets [unitX,unitY,unit_X,unit_X+unit_Y,unit_X] :: Trail V2 Double -- ここで型Annotationすべし
        p = P $ atParam t1 0.5
    in place (circle 0.05 # fc red # lw none) p <> stroke t1

dia24_4' = 
    let t1 = fromOffsets [unitX,unitY,unit_X,unit_X+unit_Y, 2 *^ unit_X, 2 *^ unitY] :: Trail V2 Double
        p = P $ atParam t1 0.5
    in pad 1.2 $ place (circle 0.05 # fc red # lw none) p <> stroke t1


-- これなんでうまくいかないんだろうか
    -- p1,p2を結んだはずのLineがdのOriginから出発するんだが。
dia24_5 =
    let d = genBCDia (fromOffsets [unitX,unitY,unit_X]) $ 4*(1+3) + (1+3)*2 :: Diagram B
        p1 = getCoor (4 :: Int) d
        p2 = getCoor (2 :: Int) d
        l = p1 ~~ p2 `at` p1 :: Located (Trail V2 Double)
        v = (0.1 *^ ) . normalize . perp $ p2 .-. p1
        p3 = atParam l 0.5
        p4 = p3 .+^ v
        lab = place (boxedText "f" 0.1) p4 -- # moveOriginTo p1
        labeledLine = lab <> stroke l -- # moveOriginTo p1
    in d <> labeledLine

-- Located Trailにラベルをつける関数の模索
    -- 正直最後のQDiagramはTrailのチョイスをミスった感あるが、Arrowにラベルをつける上では問題なく使えそう
    -- 地味にラベルの位置を微調整できるように定義した
dia24_6 =
    let f loctrl lbl n = 
            let p1 = atParam loctrl 0 :: P2 Double
                p2 = atParam loctrl 1 :: P2 Double
                p3 = atParam loctrl n :: P2 Double
                v =  (0.1 *^) . normalize . perp $ p2 .-. p1
                p4 = p3 .+^ v
            in place lbl p4 :: Diagram B
    in f (at (fromOffsets [unitX,unitY,unit_X,unitY]) origin :: Located (Trail V2 Double)) (boxedText "f" 0.1) 0.48 
      <> (strokeLocTrail (at (fromOffsets [unitX,unitY,unit_X,unitY]) origin) :: Diagram B)

-- 問題なさそうならばPartsもしくはDiagramLanguage行き
    -- 多分Partsが適する。図式言語以外でも活用できる。
    -- Partsに移行したのでこちらはコメントアウト
-- attachLabel loctrl lbl n =
--     let p1 = atParam loctrl 0 :: P2 Double
--         p2 = atParam loctrl 1 :: P2 Double
--         p3 = atParam loctrl n :: P2 Double
--         v = (0.1 *^) . normalize . perp $ p2 .-. p1
--         p4 = p3 .+^ v
--     in place lbl p4 :: Diagram B

-- 矢印にラベルをつける実験
dia24_7 =
    let loc = at (arcBetween zero unitX 0.1) origin :: Located (Trail V2 Double)
        lbl = boxedText "f" 0.1
    in attachLabel loc lbl 0.5 <> arrowFromLocatedTrail loc


-- 家系図やあみだくじ的な辺を描画する関数
dia25_1 =
    let f v = 
            let vx = project unitX v 
                vy = 0.5 *^ (project unitY v)
            in fromOffsets [vy,vx,vy]
    in strokeTrail $ f (1.3 ^& 5.4)

-- ベクトルを渡すと、その始点と終点を結ぶあみだくじの辺っぽいTrailを返す関数
    -- 二点を渡された時に、.-.と併用することであみだっぽく辺を結べる
    -- 特にsymmLayoutのRoseTreeに向いている
    -- これもParts行きか？
familytreeEdge v =
    let vx = project unitX v
        vy = 0.5 *^ (project unitY v)
    in fromOffsets [vy,vx,vy]

dia25_2 =
    let g = Alga.path [1,2,3,4] + 3*5 + 2*6 + 1*8 + 8*(9+10)
        t = genTree 1 g
        sym = symmLayout t
        amidaTrail p1 p2 = (familytreeEdge $ p2 .-. p1) `at` p1 
    in pad 1.2 $ renderTree (const bc) (\o1 o2 -> strokeLocTrail $ amidaTrail o1 o2) sym

dia25_2' = scaleY (-1) dia25_2

-- attachLabelで使った方法の抽出で、特定のPointとベクトルから0.1だけ離れた点を導出する関数
    -- Trailだけでなく一般のオブジェクトに対してラベル付けする際に利用できる
    -- subdiagramのlocationと組み合わせることで
dia25_3 =
    let f p v d =
            let v' = d *^ (normalize v)
            in p .+^ v'
    in place (circle 1) (f origin unitY 0.4) # showOrigin

-- テキストとボックスの例（Gallaryより）
-- なぜか型推論がうまくいかない
    -- height,widthのところでpaddedをpaddingにしてたのが敗因の模様。paddingはただのDouble値。
dia26_1 = do
    l2 <- lin2
    let box innards padding = 
            let padded = strutY padding 
                            ===
                        (strutX padding ||| centerXY innards ||| strutX padding)
                            ===
                        strutY padding
                height = diameter (r2 (0,1)) padded
                width = diameter (r2 (1,0)) padded
            in centerXY innards <> roundedRect width height 0.1
        textOpts n = TextOpts l2 INSIDE_H KERN False 1 n
        text' :: String -> Double -> Diagram B
        text' s n = textSVG_ (textOpts n) s # fc white # lw none :: Diagram B
        centredText ls n = vcat' (with & catMethod .~ Distrib & sep .~ n)
                            (map (\l -> centerX (text' l n)) ls) :: Diagram B
        centredText' s = centredText (splitOn "\n" s)
        padAmount = 0.5 :: Double
        down = r2 (0,-10)
        upright = r2 (7,5)
        right = r2 (15,0)
        mybox s n = (box (centredText' s 1) padAmount) # named n
        sCube = fc navy $ mconcat
            [ mybox "Permutation" "perm"
            , mybox "Permutation\ngroup" "permgroup"                     # translate right
            , mybox "Symmetry" "sym"                                     # translate upright
            , mybox "Parameterised\npermutation" "paramperm"             # translate down
            , mybox "Parameterised\npermutation\ngroup" "parampermgroup" # translate (right ^+^ down)
            , mybox "parameterised\nsymmetry" "paramsym"                 # translate (upright ^+^ down)
            , mybox "Symmetry\ngroup" "symgroup"                         # translate (upright ^+^ right)
            , mybox "Parameterised\nsymmetry\ngroup" "paramsymgroup"     # translate (down ^+^ right ^+^ upright) ] :: Diagram B
        drawLines cube = foldr (.) id (map (uncurry (connectOutside' (with
                                & headLength .~ small
                                & shaftStyle %~ lw thin))) pairs) cube :: Diagram B
                where pairs = [ ("perm", "permgroup")
                              , ("perm", "sym")
                              , ("perm", "paramperm")
                              , ("paramperm", "paramsym")
                              , ("sym", "symgroup")
                              , ("paramsym", "paramsymgroup")
                              , ("permgroup", "parampermgroup")
                              , ("peramperm","parampermgroup")
                              , ("symgroup", "paramsymgroup")
                              , ("sym", "paramsym")
                              , ("permgroup", "parampermgroup")
                              , ("parampermgroup", "paramsymgroup")]
        example = 
            let c = sCube
            in pad 1.1 . centerXY $ c <> drawLines c <> square 30
                                  # fc whitesmoke
                                  # scaleY 0.94
                                  # translateX 11
                                  # translateY (-3)
    return example

-- ちょっとした可換図式の試作
dia26_2 =
    let sh1 = boxedText "Sh(X)" 0.15
        sh2 = boxedText "Sh(Y)" 0.15
        fsup = boxedText "f^*" 0.1
        fsub = boxedText "f_*" 0.1
        d = genLabelDia (fromOffsets [unitX]) ["Sh(X)","Sh(Y)"] (1+2 :: Alga.Graph Int)
        trl = origin ~~ unitX :: Trail V2 Double
        trl1 = at trl (0 ^& 0.8)
        trl2 = at trl (0 ^& (-0.8))
        mor1 = arrowFromLocatedTrail trl1 <> attachLabel trl1 fsup 0.5
        mor2 = arrowFromLocatedTrail trl2 <> attachLabel trl2 fsub 0.5
    in d <> mor1 # translateY 0.8 <> mor2 # translateY 0.8

-- オブジェクトの境界上を結ぶLocatedTrail
dia26_3 = 
    let p2 = 2 ^& 3
        d1 = place (circle 0.1 # fc red # lw none :: Diagram B) origin
        d2 = place (square 0.2 # fc blue # lw none :: Diagram B) p2
        v = 0.5 *^ (p2 .-. origin)
        midpoint = origin .+^ v
        q1 = fromMaybe origin $ rayTraceP midpoint (negated v) d1
        q2 = fromMaybe p2 $ rayTraceP midpoint v d2
    in strokeLocTrail (q1 ~~ q2 :: Located (Trail V2 Double)) <> d1 <> d2


-- オブジェクト２つとそれぞれの中心点とラベルを渡すと、境界で出入りする矢印とその中央部分にラベルをつけて返す関数
    -- これ、あとはArrowOptsも渡せるようにすれば完成では？Monic、Epicなど具体名を指定すればその装飾がなされるようになるぞ。
-- connectTrail o1 o2 p1 p2 lbl =
--     let v = 0.5 *^ (p2 .-. p1)
--         midpoint = p1 .+^ v
--         u = 0.1 *^ (perp $ normalize v)
--         q1 = fromMaybe origin $ rayTraceP midpoint (negated v) d1
--         q2 = fromMaybe p2 $ rayTraceP midpoint v d2
--         loctrl = q1 ~~ q2 :: Located (Trail V2 Double)
--         arr = arrowFromLocatedTrail loctrl
--         lab = attachLabel loctrl (boxedText lbl 0.15) 0.5
--     in lab <> arr :: Diagram B

-- LocTrailを矢印にしてラベルもつけてみた
dia26_3' = 
    let p2 = 2 ^& 3
        d1 = place (circle 0.1 # fc red # lw none :: Diagram B) origin
        d2 = place (square 0.2 # fc blue # lw none :: Diagram B) p2
        v = 0.5 *^ (p2 .-. origin)
        u = 0.1 *^ (perp $ normalize v)
        midpoint = origin .+^ v
        q1 = fromMaybe origin $ rayTraceP midpoint (negated v) d1
        q2 = fromMaybe p2 $ rayTraceP midpoint v d2
        trl = q1 ~~ q2 :: Located (Trail V2 Double)
        arr = (arrowFromLocatedTrail trl <> attachLabel trl (boxedText "f" 0.3) 0.5) # translate u
    in arr <> d1 <> d2

dia27_1 =
    let box innards padding = 
            let padded = strutY padding 
                            ===
                        (strutX padding ||| centerXY innards ||| strutX padding)
                            ===
                        strutY padding
                height = diameter (r2 (0,1)) padded  
                width  = diameter (r2 (1,0)) padded 
            in centerXY innards <> roundedRect width height 0.1 :: Diagram B
    in box (fc red $ lw none $ strokePath $ textSVG "Test.Test.Test." 0.15) 0.2

dia27_2 =
    let labdia = (genLabelDia 
                    (fromOffsets [(sqrt 3 / 2) *^ unit_X + unit_Y , 2 *^ unitX ,(sqrt (3/2)) *^ unit_X + unitY ])
                    ["A","B","C"]
                    (1+2+3 :: Alga.Graph Int)) :: Diagram B
        f = fromMaybe (mkSubdiagram mempty) . flip lookupName labdia :: Int -> Subdiagram B V2 Double Any
        s1 = f 1
        s2 = f 2
        s3 = f 3
        c1 = location s1 
        c2 = location s2 
        c3 = location s3 
        loctrl1 = pointLocTrailOS c1 c2 s1 s2
        p1 = atParam loctrl1 0
        p2 = atParam loctrl1 1
        v1 = p2 .-. p1
        u1 = 0.07 *^ (normalize $ perp v1)
        trl = fromOffsets [u1,-u1,-u1,u1,v1] `at` p1
        d = (arrowFromLocatedTrail $ trl)
         <> (arrowFromLocatedTrail $ pointLocTrailOS c2 c3 s2 s3)
         <> (arrowFromLocatedTrail $ pointLocTrailOS c1 c3 s1 s3)
         <> labdia
    in d



dia27_3 =
    let pararr v d =
            let u = 0.1 *^ (perp $ normalize v)
                arr1 = arrowV v # translate u
                arr2 = arrowV v # translate (-u)
                d' = place d (origin .+^ (0.5 *^ v))
            in arr1 <> arr2 <> d' # scale 0.9
    in pararr (unitX +(sqrt 2) *^ unitY) reticle

vectorStoT loctrl =
    let p1 = atParam loctrl 0 :: P2 Double
        p2 = atParam loctrl 1 :: P2 Double
    in p2 .-. p1

parrarelArrows loctrl d =
    let v = vectorStoT loctrl
        u = perp $ 0.1 *^ (normalize $ v)
        loctrl1 = loctrl # translate u
        loctrl2 = loctrl # translate (-u)
        arr1 = arrowFromLocatedTrail loctrl1
        arr2 = arrowFromLocatedTrail loctrl2
        s = atParam loctrl 0 :: P2 Double
        midpoint = s .+^ (0.5 *^ v)
        d' = place d midpoint
    in arr1 <> arr2 <> d'


-- dia28_1 =
--     let d = 