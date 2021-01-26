module Parts (
    module Parts
  --, module Diagrams.Prelude EmptyがPreludeとLayout.Treeで衝突、しかしExportではhidingできないので、インポートしやすいPreludeの方を封印
  , module Data.Tree
  , module Diagrams.TwoD.Layout.Tree
  --, module Diagrams.Backend.SVG
  ,module Diagrams.Backend.PGF
  , module Data.Typeable
  , module Graphics.SVGFonts
  --, module Diagrams.Backend.PGF
  , module Data.Char
  , module Diagrams.TwoD.Arrow
) where

import Diagrams.Prelude
import Algebra.Graph hiding((===))
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import Diagrams.Backend.SVG
import Diagrams.Backend.PGF hiding(B)
--import Diagrams.Trail
import Diagrams.TwoD.Arrow
import Data.Typeable
import Diagrams.TwoD.Size
import Graphics.SVGFonts
import Data.Maybe(fromMaybe)
import Data.Char


--easyRender :: (Show n, RealFloat n) => FilePath -> QDiagram SVG V2 n Any -> IO ()
-- easyRender name diag = renderPretty ("/Users/fujimotomakoto/Documents/latexs/DailyStrategy/202011/img/" ++ name) fixedSize diag

setSize :: Num n => n -> n -> SizeSpec V2 n
setSize w h = mkSizeSpec2D (Just w) (Just h)

fixedSize :: Num n => SizeSpec V2 n
fixedSize = setSize 800 600

-- ブラウザで確認するためのお手軽レンダリング関数
renderTest = renderPretty "test.svg" (mkSizeSpec2D (Just 400) (Just 300))

-- 文字に正方形〜長方形のenvelopeを与える
boxedText xs s =
    let n = fromIntegral  (length xs)
        n1 = fromIntegral $ length $ filter (\c -> isAscii c || isDigit c || isControl c || isMark c)  xs--英数字半角を識別
        n2 = n - n1
    in text xs # fontSize (local s) <> rect ((0.73 * n1 + 1.0 * n2) * s) s # lw none

    --in text xs # fontSize (local s) # withEnvelope (rect (0.73 * n * s) s :: D V2 Double)
        -- EnvelopeはTraceと関係ないのでconnectOutsideに干渉できない。withEnvelopeは不適切
-- 標準的関数
regText xs = boxedText xs 0.2

-- 黒円。点を表す基本パーツ
    -- 半径は、矢印の長さが1であることを前提としている
bc = circle 0.02 # fc black
-- 黒円無限リスト。atPointsなどに。
bcs = repeat bc
-- グラフの頂点で名前付け
genBCs = zipWith named ([1..] :: [Int]) bcs

-- Trail系統のデータとグラフからQDiagramを構成する
    -- 対象が黒円の可換図式ができる
    -- 矢印にはラベルをつけられるだろうか？
genBCDia :: (IsName n, Renderable (Path V2 n) b , RealFloat n, Typeable n) => [Point V2 n] -> Graph Int -> QDiagram b V2 n Any
genBCDia trl g =
    let n_vs      = length $ vertexList g
        es      = edgeList g
        arrOpts = with & headLength .~  (local 0.07)
                       & gaps    .~  (local 0.03)
        arrows = foldr (.) id . map (uncurry (connectOutside' arrOpts)) $ es
        objs   = genBCs # take n_vs
    in pad 1.3 $ arrows $ atPoints trl objs
-- 頂点が黒丸の可換図式だが、作図全般で骨格をとりあえず作るのに便利なのでここに配置する

-- 木構造をAlgebraic Graphから作る
genTree :: Ord a =>  a -> Graph a -> Tree a
genTree a g = 
    case g of Algebra.Graph.Empty -> Node a []
              _     -> let es = edgeList g
                           children = map snd . filter (\t -> fst t == a) $ es
                       in  Node a (map (flip genTree g) children)


-- Forestジェネレータ
genForest :: Ord a => [a] -> Graph a -> [Tree a]
genForest as g = map (flip genTree g) as

-- これは悪ふざけ
jungle :: Ord a => Graph a -> [Tree a]
jungle g = genForest (vertexList g) g

-- genBCDiaの木構造版。ザッと構造だけすぐ見たい時に便利かも？
genBCTree n = renderTree (\_ -> bc) (~~) . symmLayout . genTree n

genLabelTree n xs = renderTree (\n -> boxedText (xs !! (n-1)) 0.2) (~~) . symmLayout . genTree n

genLabelTree' opts n f = 
    renderTree id (~~) .  symmLayout' opts . fmap f . genTree n

-- connectOutside'のアレンジ実装
    -- ラベルなどのオブジェクトを配置するための透明Trailを導出する


-- connectOutside'から、Trace上で出入りするTrailの作り方だけ抽出
    -- subdiagramの位置を取得して、その中間点から各位置にTraceを出す
    -- TracePに失敗するとsubdiagramの位置が利用される
trailOutside n1 n2 =
    withName n1 $ \b1 ->
    withName n2 $ \b2 ->
        let v = location b2 .-. location b1
            midpoint = location b1 .+^ (v ^/ 2)
            s' = fromMaybe (location b1) $ rayTraceP midpoint (negated v) b1
            e' = fromMaybe (location b2) $ rayTraceP midpoint v b2
        in atop (strokeLocTrail $ trailFromVertices [s',e'] `at` s') 

--とりあえずTrailを返す関数。Locatedにする操作と分けておく
    -- これLocatedTrailにしないとtracePした意味が無くなる
pointLocTrailOS p1 p2 o1 o2 =
    let v = p2 .-. p1
        midpoint = p1 .+^ (v ^/ 2)
        s' = fromMaybe p1 $ traceP midpoint (negated v) o1
        e' = fromMaybe p2 $ traceP midpoint v o2
    in trailFromVertices [s',e'] `at` s'

-- LocatedTrailを返す版
    -- symmLayoutのTreeの描画に有効な関数でもある
    -- 欠陥はっけん。p1をスタート地点にしてどうすんの。
-- pointLocTrailOS p1 p2 o1 o2 = pointTrailOS p1 p2 o1 o2 `at` p1

-- subdiagram関連
getCoor n = location . fromMaybe (mkSubdiagram mempty) . lookupName n


-- attachLabelでラベルのつく左右(上下)を変えられるよう拡張
    -- 末尾のb::BoolがTrueならば進行方向左、Falseならば右につく
    -- 始点と終点を結ぶよりも接線ベクトルを取得する方が絶対良いのでは？
attachLabel_ loctrl lbl asp1 asp2 b =
    let --p1 = atParam loctrl 0
        --p2 = atParam loctrl 1
        p3 = atParam loctrl asp1 -- Trail上でラベルのつく位置
        u = tangentAtParam loctrl asp1 -- ラベルのつく位置の接ベクトル
        v = if b then (asp2 *^) . normalize . perp $ u -- asp2はTrailからの距離
                 else ((-asp2) *^) . normalize . perp $ u
        p4 = p3 .+^ v
    in place lbl p4 :: Diagram PGF
    


-- LocatedTrailにラベル付けして描画する関数
    -- ただしこの関数はまだラベルを配置するだけ
    -- LocatedTrailの描画やarrowFromLocatedTrail化はこの関数と組み合わせて実行すればよし
attachLabel loctrl lbl n = attachLabel_ loctrl lbl n 0.1 True
-- 抽象化の名残
    -- let p1 = atParam loctrl 0 :: P2 Double
    --     p2 = atParam loctrl 1 :: P2 Double
    --     p3 = atParam loctrl n :: P2 Double
    --     v = (0.1 *^) . normalize . perp $ p2 .-. p1
    --     p4 = p3 .+^ v
    -- in place lbl p4 :: Diagram B


-- 中身と幅を渡すと、中心に中身を据えて周りに余白を確保した図式を返す関数
    -- GallaryのSymmetry Cubeから拝借
padded padding innards =    strutY padding
                                ===
                (strutX padding ||| centerXY innards ||| strutX padding)
                                ===
                            strutY padding


-- 長方形ラッパー。
    -- 中身のサイズにアジャストして自動でサイズ決定。
box innards padding = 
    let pad_innards = padded padding innards
        height      = diameter (r2 (0,1)) pad_innards
        width       = diameter (r2 (1,0)) pad_innards
    in centerXY innards <> rect width height 

-- 図式を入れて周りに余白を取り、長方形で囲んだ図式にする関数
roundBox innards padding = 
    let pad_innards = padded padding innards
        height      = diameter (r2 (0,1)) pad_innards
        width       = diameter (r2 (1,0)) pad_innards
    in centerXY innards <> roundedRect width height 0.05 -- pad_innardsはあくまで寸法測定用で、描画には使わない


-- 図式ちゅうのオブジェクトを名前リストで指定して、その中心に特定のオブジェクトを設置する関数
centerPlace d obj ns = 
    let ps = map (location . fromMaybe (mkSubdiagram mempty) . flip lookupName d) ns
        p0 = centroid ps
        -- u0 = p0 .-. origin
        obj' = place obj p0
    in d <> obj'

placeWithName d n obj = 
    let p = location . fromMaybe (mkSubdiagram mempty) $ lookupName n d
        obj' = place obj p
    in d <> obj'

-- =====================直線のスタイル用==================

dashed = dashingN [0.03,0.03] 0