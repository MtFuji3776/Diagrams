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
monic = 
    let tailBar = fromOffsets [0 ^& 0.2]
        morph   = arrowFromLocatedTrail (fromOffsets [1 ^& 0] ) # translateY 0.1
    in tailBar <> morph
-- connectOutsideシリーズの鏃もmonicの尻尾にできないだろうか？
    --ArrowShaftにTrailをくっつけることができるか？
    --指定した矢印のスタイルを更新してmonicにできるのが理想

-- Trailで定義すればPathの連結モノイドが導入される
    -- Arrowで使う直前にLocatedに持ち上げれば良し
monicShaft trl =
    let p1 = atParam trl 0
        p2 = atParam trl 1
        v  = p2 .-. p1 
        u  = perp . (0.07 *^) . normalize $ v :: V2 Double
        tailBar = fromOffsets [u,(-2) *^ u,u] :: Trail V2 Double
    in tailBar <> trl

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
        eqTail = fromOffsets [u,-u,u',-u'] :: Trail V2 Double
    in eqTail <> trl

open = arrowV' (with & headStyle %~ fc white . lw 0.5) unitX 

-- CoverやEpicなどのArrowHead系には、ArrowOpts単品を渡すことにする
openHead = with & headStyle %~ fc white . lw 0.3


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

-- 離散圏図式生成機。対象だけ描画する
genDiscrete trl objs g =
    let vs = vertexList g
        es = edgeList g
        objsMap = Map.fromList $ zip vs (zipWith named (vs :: [Int]) objs)
        d = atPoints trl [objsMap Map.! i | i <- vs ]
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
                 locTrail_ :: Located (Trail V2 Double)
               , arrOpts_  :: ArrowOpts Double
               , symbols_  :: [Diagram B]
               } deriving(Typeable)

instance Default MorphOpts where
    def = Morph ((mempty :: Trail V2 Double) `at` origin) with []

-- LocatedTrailを生成し、Edges値をキーとしてMapに格納する
genMorphOpts es d = 
        foldr (\(nm1,nm2) mp -> Lens.at (nm1,nm2) Lens.?~ def{locTrail_ = (mkLocTrail (nm1,nm2) d)} $ mp) Map.empty es

-- Algaから図式の抽象グラフ構造を読み取り、Trailから座標情報を読み取り、離散グラフとLocatedTrailのMapの組を作って返す
    -- このあと、LocatedTrailのMapを装飾しつつarrowFromLocatedTrailに適用し、離散圏に射を入れていく関数が続く
genGraphLocTrail trl objs g =
    let disd = genDiscrete trl objs g
        es = edgeList g
        morphmap = genMorphOpts es disd
    in (disd :: Diagram B,morphmap)

-- MorphOptsを受け取り、矢印のDiagram Bを導出する関数
genArrow (Morph loc opts symbs) =
    mconcat $ (arrowFromLocatedTrail' opts loc):symbs

mkDiagram (disd,morphmap) =
    let arrowDia = foldr (\x y -> genArrow x <> y) mempty morphmap 
    in arrowDia <> disd :: Diagram B

genDiagram trl objs g = mkDiagram $ genGraphLocTrail trl objs g :: Diagram B