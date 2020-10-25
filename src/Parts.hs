module Parts (
    module Parts
  --, module Diagrams.Prelude EmptyがPreludeとLayout.Treeで衝突、しかしExportではhidingできないので、インポートしやすいPreludeの方を封印
  , module Data.Tree
  , module Diagrams.TwoD.Layout.Tree
  , module Diagrams.Backend.SVG
  , module Data.Typeable
  , module Graphics.SVGFonts
) where

import Diagrams.Prelude
import Algebra.Graph hiding((===))
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import Diagrams.Backend.SVG
--import Diagrams.Trail
import Diagrams.TwoD.Arrow
import Data.Typeable
import Diagrams.TwoD.Size
import Graphics.SVGFonts
import Data.Maybe(fromMaybe)


--easyRender :: (Show n, RealFloat n) => FilePath -> QDiagram SVG V2 n Any -> IO ()
easyRender name diag = renderPretty ("/Users/fujimotomakoto/Documents/latexs/DailyStrategy/202010/img/" ++ name) fixedSize diag

setSize :: Num n => n -> n -> SizeSpec V2 n
setSize w h = mkSizeSpec2D (Just w) (Just h)

fixedSize :: Num n => SizeSpec V2 n
fixedSize = setSize 800 600

-- ブラウザで確認するためのお手軽レンダリング関数
renderTest = renderPretty "test.svg" (mkSizeSpec2D (Just 400) (Just 300))

-- 文字に正方形〜長方形のenvelopeを与える
boxedText xs s =
    let n = fromIntegral  (length xs)
    in text xs # fontSize (local s) <> rect (0.73 * n * s) s # lw none

    --in text xs # fontSize (local s) # withEnvelope (rect (0.73 * n * s) s :: D V2 Double)
        -- EnvelopeはTraceと関係ないのでconnectOutsideに干渉できない。withEnvelopeは不適切
-- 標準的関数
regText xs = boxedText xs 0.2

-- 黒円。点を表す基本パーツ
    -- 半径は、矢印の長さが1であることを前提としている
bc = circle 0.05 # fc black
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
            s' = fromMaybe (location b1) $ traceP midpoint (negated v) b1
            e' = fromMaybe (location b2) $ traceP midpoint v b2
        in atop (strokeTrail $ trailFromVertices [s',e']) --とりあえずTrailを返す関数。Locatedにする操作と分けておく

pointTrailOS p1 p2 o1 o2 =
    let v = p2 .-. p1
        midpoint = p1 .+^ (v ^/ 2)
        s' = fromMaybe p1 $ traceP midpoint (negated v) o1
        e' = fromMaybe p2 $ traceP midpoint v o2
    in trailFromVertices [s',e']

-- subdiagram関連
getCoor n = location . fromMaybe (mkSubdiagram mempty) . lookupName n