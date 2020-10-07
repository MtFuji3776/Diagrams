module Parts where

import Diagrams.Prelude
import Algebra.Graph


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
genBCDia trl g =
    let es     = edgeList g
        arrows = foldr (.) id . map (uncurry connectOutside) $ es
        objs   = genBCs g
    in pad 1.3 $ arrows $ atPoints trl objs