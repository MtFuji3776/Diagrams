{-# LANGUAGE TemplateHaskell #-}
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
import qualified Control.Lens as Lens ((?~),at)
import Data.Maybe (fromMaybe)


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
-- monic = 
--     let tailBar = fromOffsets [0 ^& 0.2]
--         morph   = arrowFromLocatedTrail (fromOffsets [1 ^& 0] ) # translateY 0.1
--     in tailBar <> morph
-- connectOutsideシリーズの鏃もmonicの尻尾にできないだろうか？
    --ArrowShaftにTrailをくっつけることができるか？
    --指定した矢印のスタイルを更新してmonicにできるのが理想
-- MorphOptsを評価してArrowを生成するスタイルに決定したので凍結

-- Trailで定義すればPathの連結モノイドが導入される
    -- Arrowで使う直前にLocatedに持ち上げれば良し
monicShaft trl =
    let p1 = atParam trl 0
        p2 = atParam trl 1
        v  = p2 .-. p1 
        u  = perp . (0.05 *^) . normalize $ v :: V2 Double
        tailBar = fromOffsets [u,(-2) *^ u,u] :: Trail V2 Double
        forGap = fromOffsets [0.1*^v,(-0.1) *^ v]
    in forGap <> tailBar <> trl

measureTrail trl =
    let p1 = atParam trl 0
        p2 = atParam trl 1
    in p2 .-. p1


-- イコライザの二又の尻尾
    -- Shaft中腹のドットはQDiagramの段階で合成しないといけないっぽい
equalizerShaft trl =
    let v = (0.1 *^) . normalize $ measureTrail trl
        u = v # rotateBy (3/8)
        u' = v # rotateBy (5/8)
        eqTail = fromOffsets [v,-v,u,-u,u',-u'] :: Trail V2 Double
    in eqTail <> trl

open = arrowV' (with & headStyle %~ fc white . lw 0.5) unitX 

-- CoverやEpicなどのArrowHead系には、ArrowOpts単品を渡すことにする
openHead = with & headStyle %~ fc white . lw 0.3

cover = headStyle %~ fc white . lw 0.5


-- 図式言語

-- 量化記号を表す型
data Quantification = NoLine Double | NoQuant Double | Forall Double | Exists Double | Only Double | ExistsOnly Double deriving(Show,Eq)



evalQF (NoLine d)         = mempty
evalQF (NoQuant d)        = mempty
evalQF (Forall d)         = boxedText "∀" d
evalQF (Exists d)         = boxedText "∃" d
evalQF (Only d)           = boxedText "!" d
evalQF (ExistsOnly d)     = boxedText "∃!" d
-- 罫線
    -- lは長さの値、xは上に付ける量化記号と!
    -- translateYの値は議論の余地あり
    -- 図式の列を与えられたときに罫線の長さを自動で導出させられたらいい感じなのだが。

vline l q = 
    let line l = vrule l # alignB # translateY ((-0.2 * l))
    in if (q 0.2) == NoLine 0.2
        then mempty
        else beside (0 ^& 1) (line l) (evalQF (q 0.2))
                    

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


-- =================================================図式言語生成用の主要関数==================================================================

-- 離散圏図式生成機。対象だけ描画する
genDiscrete trl objs g =
    let vs = vertexList g :: [Int]
        --es = edgeList g
        ns = [1..] :: [Int]
        pointMap = Map.fromList $ zip ns trl
        objsMap = Map.fromList $ zip ns (zipWith named ns objs)
        checkObjs = fromMaybe mempty
        checkPoints = fromMaybe origin
        d = mconcat [place (checkObjs $ objsMap ^. Lens.at i) (checkPoints $ pointMap ^. Lens.at i)  | i <- vs ]
    in d

-- 名前二つとQDiagramからLocatedTrailを導出
mkLocTrail (nm1,nm2) d =
    let findSub = fromMaybe (mkSubdiagram mempty) . flip lookupName d
        sub1 = findSub nm1
        sub2 = findSub nm2
        p1 = location sub1
        p2 = location sub2
    in pointLocTrailOS p1 p2 sub1 sub2



-- genGraphLocTrailが生成するのはLocatedTrailのMapではなくMorphOptsのMapであるべきだ
    -- arrOpts_,symbols_はwith,[]という初期値で、locTrail_だけ計算結果を入れる
    -- arrOpts_,symbols_の更新を次の関数で実施
data MorphOpts = Morph{ 
                 _locTrail :: Located (Trail V2 Double)
               , _arrOpts  :: ArrowOpts Double
               , _symbols  :: [Diagram B]
               } 
            --    | Twin{
            --      _leftMorph :: MorphOpts
            --    , _rightMorph :: MorphOpts 
            --    }
               deriving(Typeable)

instance Default MorphOpts where
    def = Morph ((mempty :: Trail V2 Double) `at` origin) with []

$(makeLenses ''MorphOpts)

-- 平行な射を扱うためにMapのキーを拡張する
    -- Twinはコンストラクタによってのみ生成される
    -- ３つ以上の平行な射にも対応すべきか検討してみよう
data KeyOfMorph = Single Int Int
                | Twin Int Int Bool
                deriving (Eq,Ord,Show)

-- LocatedTrailを生成し、Edges値をキーとしてMapに格納する
genMorphOpts es d = 
    let insert' (i,j) mp = 
            let opts = def & locTrail .~ mkLocTrail (i,j) d
            in Lens.at (Single i j) Lens.?~ opts $ mp
    in foldr insert' Map.empty es

-- Algaから図式の抽象グラフ構造を読み取り、Trailから座標情報を読み取り、離散グラフとLocatedTrailのMapの組を作って返す
    -- このあと、LocatedTrailのMapを装飾しつつarrowFromLocatedTrailに適用し、離散圏に射を入れていく関数が続く
genGraphLocTrail trl objs g =
    let disd = genDiscrete trl objs g
        es = edgeList g
        --vertexMap = Map.fromList $ zipWith named ([1..] :: [Int]) objs
        morphmap = genMorphOpts es disd
        morphmap' = fmap (over (arrOpts.gaps) (+ local 0.05)) morphmap 
    in (disd :: Diagram B,morphmap')

-- MorphOptsを評価して、ArrowのQDiagramを生成する関数
evalMorphOpts (Morph loc opts symbs) =
    mconcat $ (arrowFromLocatedTrail' opts loc):symbs

mkDiagram (disd,morphmap) =
    let arrowDia = foldr (\x y -> evalMorphOpts x <> y) mempty morphmap 
    in arrowDia <> disd :: Diagram B

genDiagram trl objs g = mkDiagram $ genGraphLocTrail trl objs g :: Diagram B


-- attachLabelのアレンジ
takeLabel_ l asp1 asp2 b opts =
    let trl = opts ^. locTrail
        lab = attachLabel_ trl l asp1 asp2 b
    in over symbols (lab:) opts

-- 簡易版
takeLabel l b = takeLabel_ l 0.5 0.1 b

buildLocTrail someFuncOnTrail loctrl =
    let p0 = atParam loctrl 0
    in flip at p0 . someFuncOnTrail . unLoc $ loctrl

-- monicの定形ジェネレータ
monic mopts = mopts & locTrail %~ (buildLocTrail monicShaft) 

-- イコライザの矢印の定形ジェネレータ
    -- 点を打つ位置が13/14というのは、equalizerShaftの都合上こうなっている
    -- ArrowOptsのうちgapシリーズは、始点と終点でTrailの接ベクトルを求めて、それと垂直方向に図形の始点を動かす
    -- Trailの初動が矢印と別を向いている場合、gapの取り方が明後日を向いてしまう
    -- 対策として、Shaftにはノータッチで飽くまでShaftの変形もQDiagramのatopで済ませるという案がある
equalizer mopts = 
    let dot = takeLabel_ (bc # scale 0.2) (13/14) 0.05 
    in mopts & locTrail %~ (buildLocTrail equalizerShaft)
             & dot True . dot False
             & arrOpts.tailGap .~ (local 0.05)
--
-- =======================以下、罫線の構築に関する関数==========================================
-- 

heightOfVline = diameter (r2 $ 0 ^& 1) . padded 0.2

-- 線のながさと量化子リストを受け取って、罫線リストを返す
verticals h = map (vline h) 


--
-- =========図式言語生成関数====================================================
--

-- 量化子リストと図式リストを受け取って図式言語一つの図式を生成する関数
diagramLanguage qs ds =
    let height = foldr max 0 . map heightOfVline $ ds
        vlines = verticals height qs
    in foldr (|||) mempty $ zipWith (|||) vlines ds