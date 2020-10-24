module DiagramLanguage where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Vector
import Graphics.SVGFonts
import Algebra.Graph hiding((===))
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import Parts
import qualified Data.Map as Map


-- ラベルにグラフの頂点で名前をつける
genLabels xs g = 
    let vs = vertexList g
    --in zipWith named vs (map (\x -> boxedText x 0.2) xs)
    in zipWith named vs (map (\x -> boxedText x 0.12 # centerXY) xs)

-- genBCDiaの仕様をちょっと変更し、ラベル用の文字列リストを受け取るようになっている
    -- 文字ラベルはatPointsに渡す[Point]の順序と対応させる必要がある
    -- Stringリストを渡す仕様上、黒円と文字ラベルが混じり合った図式は作れない
    -- そっちは別に定義する必要ありか。
genLabelDia trl xs g =
    let vs = vertexList g
        es = edgeList g
        arrOpts = with & headLength .~ (local 0.05)
                       & gaps       .~ (local 0.03)
        arrows = foldr (.) id . map (uncurry (connectOutside' arrOpts)) $ es
        objs = genLabels xs g
        labelmap = Map.fromList $ zip vs objs
    in pad 1.3 $ arrows $ atPoints trl [labelmap Map.! k | k <- vs]



-- 特別な形状の射を示す矢印
-- monicの見本
monic = 
    let tailBar = fromOffsets [0 ^& 0.2]
        morph   = arrowFromLocatedTrail (fromOffsets [1 ^& 0] ) # translateY 0.1
    in tailBar <> morph
-- connectOutsideシリーズの鏃もmonicの尻尾にできないだろうか？
    --ArrowShaftにTrailをくっつけることができるか？
    --指定した矢印のスタイルを更新してmonicにできるのが理想

open = arrowV' (with & headStyle %~ fc white . lw 0.5) unitX 


-- 図式言語

-- 量化記号を表す型
data Quantification = NoQuant Double | Forall Double | Exists Double | ExistsOnly Double deriving(Show)

evalQF (NoQuant d)        = mempty
evalQF (Forall d)         = boxedText "∀" d
evalQF (Exists d)         = boxedText "∃" d
evalQF (ExistsOnly d)     = boxedText "∃!" d
-- 罫線
    -- lは長さの値、xは上に付ける量化記号と!
    -- translateYの値は議論の余地あり
    -- 図式の列を与えられたときに罫線の長さを自動で導出させられたらいい感じなのだが。
vline l q = 
    let line l = vrule l # alignB # translateY ((-0.2 * l))
    in  beside (0 ^& 1) (line l) (evalQF (q 0.2))
                    

-- 量化記号はboxedTextで定義
    -- サイズはとりあえず0.2で様子見
-- quant s = boxedText s 0.2

-- レティクル記号
reticle = mconcat [ (0 ^& 0) ~~ (0.05^&0), (0.15 ^& 0) ~~ (0.2 ^& 0)] # translateX (-0.1) <> mconcat [(0 ^& 0) ~~ (0 ^& 0.05), (0 ^& 0.15) ~~ (0 ^& 0.2)] # translateY (-0.1)

-- 引き戻し記号
plb = fromOffsets [unitX,unitY] # scale (1/4)

-- 積記号
prd = fromOffsets [2*^unitX , unit_X , unitY , 2 *^ unit_Y] # scale (1/4)

-- 同型射を表す記号
sim = cubicSpline False [0^&0,1^&0.2, 2^&0, 3^&(-0.2) , 4^&0]
    # scale (1/8)
    # translateX 0.25 
    # translateY 0.05

-- X方向の単位ベクトルの同型射サンプル
isomUnitX = dia21_1 <> arrowV unitX
    where dia21_1 = cubicSpline False [0^&0,1^&0.2, 2^&0, 3^&(-0.2) , 4^&0]
                  # scale (1/8)
                  # translateX 0.25 
                  # translateY 0.05

-- example
example1 =
    let g1 = genBCDia (triangle 1) (2 * (1+3) + 3*1)
        g2' = genBCDia (regPoly 1 1) 4 
        g2 = connectOutside (4 :: Int) (2 :: Int) $ g1 <> g2' # translateY 1.2 
    in hsep 0.05 [vline 2 (Forall) , g1, vline 2 Exists, g2]

