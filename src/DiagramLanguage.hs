{-# LANGUAGE TemplateHaskell #-}
module DiagramLanguage where

import Diagrams.Prelude
import Diagrams.Backend.SVG 
import Diagrams.Backend.PGF hiding (B)
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Vector
import Graphics.SVGFonts
import Algebra.Graph hiding((===))
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import Parts hiding (B)
import qualified Data.Map as Map
import qualified Control.Lens as Lens ((?~),at)
import Data.Maybe (fromMaybe,isNothing)
import Data.Char
import Diagrams.TwoD.Image(loadImageEmb,image)
import Data.Either(fromRight)
import Aeson
-- import Diagrams.Backend.Rasterific
-- import CmSymbols



-- 課題：PGFバックエンドとSVGバックエンドで統一的に関数を定義できないか考えてみること
--      それができれば縮尺の違いはバックエンドの違いで場合分けして対処できる

-- -- ラベルにグラフの頂点で名前をつける
-- genLabels xs g = 
--     let vs = vertexList g
--     --in zipWith named vs (map (\x -> boxedText x 0.2) xs)
--     in zipWith named vs (map (\x -> boxedText x 0.12 # centerXY) xs)

-- -- genBCDiaの仕様をちょっと変更し、ラベル用の文字列リストを受け取るようになっている
--     -- 文字ラベルはatPointsに渡す[Point]の順序と対応させる必要がある
--     -- Stringリストを渡す仕様上、黒円と文字ラベルが混じり合った図式は作れない
--     -- そっちは別に定義する必要ありか。
-- genLabelDia trl xs g =
--     let vs = vertexList g
--         es = edgeList g
--         arrOpts = with & headLength .~ (local 0.05)
--                        & gaps       .~ (local 0.03)
--         arrows = foldr (.) id . map (uncurry (connectOutside' arrOpts)) $ es
--         objs = genLabels xs g
--         labelmap = Map.fromList $ zip vs objs
--     in pad 1.3 $ arrows $ atPoints trl [labelmap Map.! k | k <- vs]





-- ============================== hboxOnlineによるLaTeX文字生成関連 =========================================
--
between xs ys zs = xs <> zs <> ys

mathEnv = between "$" "$"

getPGFSymbol d xs = do
    lab <- hboxOnline . mathEnv $ xs
    return $ lab # scale d # centerXY

--getPGFLabel = getPGFSymbol 0.015

getPGFObj = getPGFSymbol 0.015

--getPGFText xs = centerXY . scale 0.01 <$> hboxOnline xs

--
-- ============================== 図式言語 ==========================================
--

-- 量化記号を表す型
data Quantification = NoLine | NoQuant | Forall | Exists | Only | ExistsOnly deriving(Show,Eq)

forall     = getPGFObj "\\forall"
exists     = getPGFObj "\\exists"
existsOnly = getPGFObj "\\exists !"

evalQF NoLine         = scale 1.2 <$> return mempty
evalQF NoQuant        = scale 1.2 <$> return mempty
evalQF Forall         = scale 1.2 <$> forall
evalQF Exists         = scale 1.2 <$> exists
evalQF Only           = scale 1.2 <$> getPGFObj "!"
evalQF ExistsOnly     = scale 1.2 <$> existsOnly
-- 罫線
    -- lは長さの値、xは上に付ける量化記号と!
    -- translateYの値は議論の余地あり
    -- 図式の列を与えられたときに罫線の長さを自動で導出させられたらいい感じなのだが。

vline l q = do
    let line l = vrule l # alignB # translateY ((-0.2 * l))
    if q == NoLine
        then return mempty
        else beside (0 ^& 1) (strutY 0.1 === line l) <$> evalQF q

                    

-- 量化記号はboxedTextで定義
    -- サイズはとりあえず0.2で様子見
-- quant s = boxedText s 0.2

-- レティクル記号
reticle = scale (0.5) 
        $ mconcat [ (0 ^& 0) ~~ (0.05^&0), (0.15 ^& 0) ~~ (0.2 ^& 0)] # translateX (-0.1) <> mconcat [(0 ^& 0) ~~ (0 ^& 0.05), (0 ^& 0.15) ~~ (0 ^& 0.2)] # translateY (-0.1)

-- 引き戻し記号
--plb = fromOffsets [unitX,unitY] # scale (1/4)

pullback_ lctrl1 lctrl2 = let 
    -- 直線状LocatedTrailの長さを求める関数。arcLengthの方がより汎用的と思われる。
    loclength :: Located (Trail V2 Double) -> Double
    loclength lc = let 
        s = atParam lc 0
        t = atParam lc 1
        v = s .-. t
        in norm v :: Double
    d1 = loclength lctrl1
    d2 = loclength lctrl2
    -- LocatedTrailの長さに応じて、長い方と短い方を抽象的に指定する。choiceはそれをさらに抽象化したもの。
    choice b = if b then lctrl1 else lctrl2
    shortOne = choice (d1 < d2)
    longOne =  choice (d2 < d1)
    -- 短い方のLocatedTrailの始点と終点をp0,p1とし、長い方のそれをq0,q1とする。
    (p0,p1) = (atParam shortOne 0,atParam shortOne 1)
    (q0,q1) = (atParam longOne 0,atParam longOne 1)
    -- 短い方のLocatedTrailを主体として、引き戻し記号の端点pを決める。これにより、引き戻し記号の一辺の長さが決定する。
    v = 1/4 *^ (p1 .-. p0)
    p = p0 .+^ v
    -- 長い方のLocatedTrail上に端点を取る際は、始点からの距離がp0,p間距離と一致するようパラメータを計算してとる。dist p0 pでdist q0 q1を割れば、適切なパラメータが計算できる。
    dist = norm v
    whole = norm $ q1 .-. q0
    prm = dist / whole
    q = atParam longOne prm
    -- p,qの中点を取得し、そこから線分pqの法線ベクトルを取る。法線ベクトルを適切に拡大して、p,mid,qが直角二等辺三角形を成すよう計算する。これにより、引き戻し記号は直角になる。
    z = 1/2 *^ (p .-. q)
    mid_ = q .+^ z
    l = norm z
    u = normalize . perp $ z
    w = l *^ u
    mid = mid_ .+^ w
    -- 三点p,mid,qを結んで完成。
    in lw thin $ p ~~ mid <> mid ~~ q

pullback (n1,n2) (m1,m2) mp = let 
    getLocTrail (n,m) mp = fromMaybe (mempty `at` origin) $ view (Lens.at $ Single n m) mp
    lctrl1 = getLocTrail (n1,n2) mp
    lctrl2 = getLocTrail (m1,m2) mp
    in pullback_ lctrl1 lctrl2



-- 積記号
prd = fromOffsets [2*^unitX , unit_X , unitY , 2 *^ unit_Y] # scale (1/4)


-- X方向の単位ベクトルの同型射サンプル
isomUnitX = dia21_1 <> arrowV unitX
    where dia21_1 = cubicSpline False [0^&0,1^&0.2, 2^&0, 3^&(-0.2) , 4^&0]
                  # scale (1/8)
                  # translateX 0.25 
                  # translateY 0.05

-- example
example1 = do
    let g1 = genBCDia (triangle 1) (2 * (1+3) + 3*1)
        g2' = genBCDia (regPoly 1 1) 4 
        g2 = connectOutside (4 :: Int) (2 :: Int) $ g1 <> g2' # translateY 1.2 
    v1 <- vline 2 Forall
    v2 <- vline 2 Exists
    return $ hsep 0.05 [v1, g1, v2, g2]


-- =================================================図式言語生成用の主要関数==================================================================

-- Jsonからデコードするオブジェクト用のデータ
type ObjectSource = (Int,((Double,Double),String))

-- 対象のJsonデータから対象を生成する関数
    -- これをmapする関数を定義すればgenDiscreteのJson対応版になる
genObject :: ObjectSource -> OnlineTex (Diagram PGF)
genObject (n,((x,y),str)) = do
    obj' <- getPGFObj str
    let obj = named n obj'
        p = p2 (x,y)
    return $ place obj p

-- Jsonファイル由来のデータを引数にとって離散圏図式を生成する関数
genDiscreteFromJson :: [ObjectSource] -> OnlineTex (Diagram PGF)
genDiscreteFromJson objs = 
    let dias' = sequence . map genObject $ objs :: OnlineTex [Diagram PGF]
        dias = fmap (foldr (<>) mempty) dias' :: OnlineTex (Diagram PGF)
    in dias


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


-- 平行な射を扱うためにMapのキーを拡張する
    -- Twinはコンストラクタによってのみ生成される
    -- ３つ以上の平行な射にも対応すべきか検討してみよう
data KeyOfMorph = Single Int Int
                | Twin Int Int Bool
                deriving (Eq,Ord,Show)

-- genGraphLocTrailが生成するのはLocatedTrailのMapではなくMorphOptsのMapであるべきだ
    -- arrOpts_,symbols_はwith,[]という初期値で、locTrail_だけ計算結果を入れる
    -- arrOpts_,symbols_の更新を次の関数で実施
data MorphOpts = Morph{ 
                 _locTrail :: Located (Trail V2 Double)
               , _arrOpts  :: ArrowOpts Double
               , _symbols  :: [Diagram PGF]
               , _actions  :: [Diagram PGF -> Diagram PGF]
               } 
            --    | Twin{
            --      _leftMorph :: MorphOpts
            --    , _rightMorph :: MorphOpts 
            --    }
               deriving(Typeable)

instance Default MorphOpts where
    def = Morph ((mempty :: Trail V2 Double) `at` origin) with [] []

$(makeLenses ''MorphOpts)


-- オブジェクト内部から輪郭に向けてレイを飛ばし、境界上の点を得る関数
    -- nはname,aはangle
    -- aで指定した角度(rotateByによるtau角度)の方向にレイが飛ぶ
anglePoint n a d =
    let sub = fromMaybe (mkSubdiagram mempty) $ lookupName n d
        p   = location sub 
        p'  = fromMaybe p (rayTraceP p (unitX # rotateBy a) sub)
    in p'

-- endmorphismのTrailのモデル
    -- arcで実装した方が良い
-- loopMorph p =
--     let trl = arc (direction unitX) (3/4 @@ turn) # scale 0.1 . rotateBy (5/8)
    -- let trl = reverseTrail . Trail . onLineSegments init $ (pentagon 0.2)
    --     ps = trailVertices $ at trl origin
    --     loop = cubicSpline False ps
    -- in loop `at` p

-- LocatedTrailを生成し、Edges値をキーとしてMapに格納する
genMorphOpts es d = 
    let insert' (i,j) mp = 
            if i == j 
                then
                    -- let p = anglePoint i (1/3) d
                    --     opts = def & locTrail .~ loopMorph p
                    -- in Lens.at (Single i j) Lens.?~ opts $ mp
                    let p = anglePoint i (1/3) d
                        opts = def & locTrail .~ (arc (direction unitX) ((-4/5) @@ turn) # scale (-0.1) . rotateBy (1/8)) `at` p
                    in Lens.at (Single i j) Lens.?~ opts $ mp
                else
                    let opts = def & locTrail .~ mkLocTrail (i,j) d
                                   & arrOpts.headLength .~ (local 10)
                    in Lens.at (Single i j) Lens.?~ opts $ mp
    in foldr insert' Map.empty es

-- Algaから図式の抽象グラフ構造を読み取り、Trailから座標情報を読み取り、離散グラフとLocatedTrailのMapの組を作って返す
    -- このあと、LocatedTrailのMapを装飾しつつarrowFromLocatedTrailに適用し、離散圏に射を入れていく関数が続く
    -- SVG専用になってしまってるのが癪。バックエンドを抽象化できないものか？
genGraphLocTrail trl objs g =
    let disd = genDiscrete trl objs g
        es = edgeList g
        --vertexMap = Map.fromList $ zipWith named ([1..] :: [Int]) objs
        morphmap = genMorphOpts es disd
        morphmap' = fmap (over (arrOpts.gaps) (+ local 0.03) . set (arrOpts.headLength) (local 0.05)) morphmap 
    in (disd :: Diagram PGF,morphmap')

-- MorphOptsを評価して、ArrowのQDiagramを生成する関数
evalMorphOpts (Morph loc opts symbs acts) =
    let protoarr = mconcat $ (arrowFromLocatedTrail' opts loc):symbs
    in foldr (.) id acts $ protoarr

mkDiagram (disd,morphmap) =
    let arrowDia = foldr (\x y -> evalMorphOpts x # lw veryThin <> y) mempty morphmap 
    in arrowDia <> disd :: Diagram PGF


genDiagram trl objs update = mkDiagram . over _2 update . genGraphLocTrail trl objs 


-- attachLabelのアレンジ
takeLabel_ l asp1 asp2 b opts =
    let trl = opts ^. locTrail
        lab = attachLabel_ trl l asp1 asp2 b
    in over symbols (lab <|) opts

-- 簡易版
    -- asp1も毎回書いてもいいかも。
takeLabel l asp1 b = takeLabel_ l asp1 0.1 b


-- MorphOptsのMapに対する作用素
    -- (i,j)をキーに持つArrowOptsにアクセスして、MorphOpts -> MorphOpts関数を適用
    -- 必然的に、射に装飾を施す関数はMorphOpts -> MorphOptsの形にすることが規格となる
    -- 無印はSingleと対応。_Twin付きはTwinキーと対応。
actOpt i j f = over (Lens.at (Single i j)) (fmap f)
-- Twin用
actOpt_Twin i j b f = over (Lens.at (Twin i j b)) (fmap f)

-- actOptとtakeLabelの併せ
    -- takeLabelはMorphOpts上の関数
    -- MorphOpts上の関数として定義→actOptでMap上に持ち上げ
tackLabel i j l b = if i /= j 
                    then actOpt i j (takeLabel l 0.5 b)
                    else actOpt i j (takeLabel_ l 0.38 0.23 b)
tackLabel_ i j l b d1 d2 = actOpt i j (takeLabel_ l d1 d2 b)

tackLabelTwin i j b1 l b2 = actOpt_Twin i j b1 (takeLabel l 0.5 b2)
takeLabelTwin_ i j b1 l b2 d1 d2 = actOpt_Twin i j b1 (takeLabel_ l d1 d2 b2)


buildLocTrail someFuncOnTrail loctrl =
    let p0 = atParam loctrl 0
    in flip at p0 . someFuncOnTrail . unLoc $ loctrl

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
-- monicShaft trl =
--     let p1 = atParam trl 0
--         p2 = atParam trl 1
--         v  = p2 .-. p1 
--         u  = perp . (0.05 *^) . normalize $ v :: V2 Double
--         tailBar = fromOffsets [u,(-2) *^ u,u] :: Trail V2 Double
--         forGap = fromOffsets [0.1*^v,(-0.1) *^ v]
--     in forGap <> tailBar <> trl
-- atParamによるtackLabelを狂わせるので凍結＆monic修正。
    -- monicの尻尾はQDiagramをatopすることにした。イコライザの尻尾もそうするべきかも。

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

cover mopts = mopts & arrOpts . headStyle %~ fc white . lw 0.5
-- monicの定形ジェネレータ

monic mopts = 
    let trl = view locTrail mopts
        d   = norm $ (atParam trl 1) .-. (atParam trl 0)
        p0  = atParam trl 0
        u0  = (0.05 *^) . normalize . perp $ tangentAtStart trl
        line = fromOffsets [u0,(-2) *^ u0,u0]
        monicTrail = (fromOffsets [tangentAtStart trl,-(tangentAtStart trl)] <> line <> unLoc trl) `at` p0
        --monicTail = place line (p0 .+^ (-0.7 *^ perp u0)) -- ArrowHTのGapの分だけ微調整。正直美しくない
    in mopts & locTrail .~ monicTrail




-- epicを表すためのArrowHead値
epicHead x y = 
    let p = fst $ dart x y
        trl = mconcat . map unLoc $ pathTrails p
        l   = diameter unitX p
        p2 = Path [trl `at` origin,trl `at` (0.8*l) *^unitX]
    in (p2,snd $ dart x y)
 
-- MorphOpts -> MorphOpts値に仕立てる
    -- よくわからんがheadGapが異常にデカく出るんだが。
    -- Envelopeのデカさかと思ったがheadGapは関係ないはず
    -- とりあえず応急処置としてheadGapをlocal (-0.03)ほど補正しているが、根本的な解決を望んでいる
epic = over arrOpts (set arrowHead epicHead)

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

-- 同型射を表す記号
sim = cubicSpline False [0^&0,1^&0.2, 2^&0, 3^&(-0.2) , 4^&0]
    # scale (1/16)
    # centerXY
    # translateY 0.05
    # lw veryThin

angledSim lctrl = let 
    vt = tangentAtParam lctrl 0.5
    a = signedAngleBetween vt unitX
    in rotate a (scale 0.5 $ sim :: Diagram PGF)

isom mopts = 
    let trl = view locTrail mopts
        midp = atParam trl 0.5
        sim' = place (angledSim trl) midp
    in mopts & symbols %~ (sim':)



-- 多くの亜種を持つtwin関数系統の一番抽象的なやつ
    -- objはとりあえず単品で渡したが、複数のオブジェクトを設置したい場合はどうすれば良いか？
    -- 図式内の既存のTrailを有効活用するためのプログラムを別途用意した方が良い気がする
        -- つーかあるだろ、それで図式の矢印のTrailを作ってるじゃねーか。
    -- あるいは既存のMorphOptsを利用してもよし
    -- 配置したいオブジェクトに一番近いLocatedTrailを特定して、attachLabelの要領で配置する。イコライザ記号のやり方。
    -- このやり方でProductとかPullbackとかmonicとかreticleとかみんな設置できるのでは？
-- twin_ i j n obj b' xmap = -- b'はBoolで、reverseの有無の確認
--     let e = view (Lens.at (Single i j)) xmap
--         setTwinKeys = set (Lens.at (Twin i j True)) e . set (Lens.at (Twin i j False)) e . sans (Single i j)
--         movetrl b opts = 
--             let trl = view locTrail opts
--                 u = n *^ normalAtParam trl 0
--                 trans   = if b then translate u . reverseLocTrail
--                                else translate (-u) 
--             in over locTrail trans opts
--         lens_Map b = over (Lens.at (Twin i j b)) (fmap (movetrl b))
--         obj' = --平行射のど真ん中にオブジェクト設置。reticleとか、随伴の記号とか。
--             let e' = view locTrail . fromMaybe def $ e
--                 p = atParam e' 0.5
--             in place obj p
--         xmap' = lens_Map True 
--               . (if b' then lens_Map False else lens_Map True)
--               . over (Lens.at (Twin i j True)) (fmap (over symbols (obj' <|)))
--               . setTwinKeys 
--               $ xmap
--     in if isNothing e 
--         then xmap
--         else xmap'

-- 通常のtwin
-- twin i j n = twin_ i j n mempty True
-- もうちょっと洗練させた関数に仕上げたのでそちらを採用する

introTwin i j mp = 
    let mopt = view (Lens.at $ Single i j) mp
        moveTrail n mop = 
            let lt = mop ^. locTrail
                u = n *^ normalAtParam lt 0
            in mop & locTrail %~ translate u
        result = 
            case mopt of Nothing -> mp;
                          Just mopt' ->
                                mp  & (Lens.at $ Twin i j True) ?~ (moveTrail (-0.05) mopt')
                                    & (Lens.at $ Twin i j False) ?~ (moveTrail 0.05 mopt')
                                    & sans (Single i j)
    in result


-- ================Arc生成関数
-- Parameterインスタンスと曲率から曲線トレイルを生成
mkArc_ trl n =
    let p1 = atParam trl 0
        p2 = atParam trl 1
    in arcBetween p1 p2 n

-- moptsからLocatedTrailを取り出し、曲率nの曲線トレイルに置き換える関数
mkArc n mopts =
    let trl = view locTrail mopts
    in mopts & locTrail .~ mkArc_ trl n

--
-- =======================以下、罫線の構築に関する関数==========================================
-- 

-- 図式リストを受け取り、縦方向のサイズをそれぞれ測定する関数
heightOfVline = diameter (r2 (0,1)) -- . padded 0.2

-- 線のながさと量化子リストを受け取って、罫線リストを返す
verticals h = mapM (vline h) 


--
-- =========図式言語生成関数====================================================
--

-- 量化子リストと図式リストを受け取って図式言語一つの図式を生成する関数
diagramLanguage qs ds = do
    let ds' = (map getEnvelope ds) :: [Envelope V2 Double]
        height = foldr max 0 . map heightOfVline $ ds'
    vlines <- verticals height qs # fmap (map alignB)
    return $ foldr (|||) mempty $ zipWith (|||) vlines (map alignB ds)


--
-- ===============================SVGFontsによる文字記号生成関数関連===========================================
--
-- PGFバックエンドをメインにしたので一旦棚上げ

-- svgObject = lw none . flip Parts.box 0.01 . fc black . strokeP . flip textSVG 0.15

-- -- 英数字に関してフォントを使い分けてくれる関数
-- switcher x
--     | isAlpha x  = mathAlphabet [x]
--     | isNumber x = mathNumber [x]
--     | otherwise  = return mempty

-- mathObject xs = lw none . flip Parts.box 0.01 . scale 0.15 . fc black <$> switcher xs


-- svgLabel = lw none . fc black . strokeP . flip textSVG 0.14


--
-- -- ============================== hboxOnlineによるLaTeX文字生成関連 =========================================
-- --
-- between xs ys zs = xs <> zs <> ys

-- mathEnv = between "$" "$"

-- getPGFSymbol d xs = do
--     lab <- hboxOnline . mathEnv $ xs
--     return $ lab # scale d # centerXY

-- getPGFLabel = getPGFSymbol 0.01

-- getPGFObj = getPGFSymbol 0.015
