module FreeNote.FN202010 where

import Parts 
import DiagramLanguage
import qualified Algebra.Graph as Alga hiding((===))
import Diagrams.Prelude
import Data.Maybe (fromMaybe)


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
        label = beside (unit_X + unitY) c_blue (boxedText "f" 0.1)
    in g1 <> strokeTrail l_red # lc red <> c_blue <> label
