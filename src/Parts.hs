module Parts (
    module Parts
  --, module Diagrams.Prelude EmptyがPreludeとLayout.Treeで衝突、しかしExportではhidingできないので、インポートしやすいPreludeの方を封印
  , module Data.Tree
  , module Diagrams.TwoD.Layout.Tree
  , module Diagrams.Backend.SVG
  , module Data.Typeable
) where

import Diagrams.Prelude
import Algebra.Graph hiding((===))
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import Diagrams.Backend.SVG
import Data.Typeable
import Diagrams.TwoD.Size


--easyRender :: (Show n, RealFloat n) => FilePath -> QDiagram SVG V2 n Any -> IO ()
easyRender name diag = renderPretty ("/Users/fujimotomakoto/Documents/latexs/DailyStrategy/202010/img/" ++ name) fixedSize diag

setSize :: Num n => n -> n -> SizeSpec V2 n
setSize w h = mkSizeSpec2D (Just w) (Just h)

fixedSize :: Num n => SizeSpec V2 n
fixedSize = setSize 400 300

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
