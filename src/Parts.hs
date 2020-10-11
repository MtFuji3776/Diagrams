module Parts where

import Diagrams.Prelude
import Algebra.Graph
import Diagrams.Backend.SVG
import Data.Typeable


-- ブラウザで確認するためのお手軽レンダリング関数
renderTest = renderPretty "test.svg" (mkSizeSpec2D (Just 400) (Just 300))

-- 文字に正方形〜長方形のenvelopeを与える
boxedText xs s =
    let n = fromIntegral $ length xs
    in text xs # fontSize (local s) # withEnvelope (rect (0.73 * n * s) s :: D V2 Double)
-- 標準的関数
regText xs = boxedText xs 0.2

-- 黒円。点を表す基本パーツ
    -- 半径は、矢印の長さが1であることを前提としている
bc = circle 0.05 # fc black
-- 黒円無限リスト。atPointsなどに。
bcs = repeat bc
-- グラフの頂点で名前付け
genBCs g = let xs = vertexList g in zipWith named xs bcs

-- Trail系統のデータとグラフからQDiagramを構成する
    -- 対象が黒円の可換図式ができる
    -- 矢印にはラベルをつけられるだろうか？
genBCDia :: (IsName n, Renderable (Path V2 n) b , RealFloat n, Typeable n) => [Point V2 n] -> Graph n -> QDiagram b V2 n Any
genBCDia trl g =
    let es     = edgeList g
        arrows = foldr (.) id . map (uncurry connectOutside) $ es
        objs   = genBCs g
    in pad 1.3 $ arrows $ atPoints trl objs
-- 頂点が黒丸の可換図式だが、作図全般で骨格をとりあえず作るのに便利なのでここに配置する