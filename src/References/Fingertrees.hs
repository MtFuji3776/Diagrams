module TextDiagrams.Fingertrees where

import Data.Typeable
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.PGF
import Diagrams.TwoD.Size
import Diagrams.TwoD.Layout.Tree
import Data.Tree
import qualified Data.Set as Set
import Data.Time
import Diagrams.TwoD.Vector
import Tutorial
import Control.Lens hiding((#),none)
--import Guide_Arrows(renderTest)
import qualified Graphics.SVGFonts as SF

-- このディレクトリ中のtest.svgに試し出力
renderTest = renderPretty "test.svg" fixedSize

-- 正三角形を回す演算
rotateRegTri s a = triangle s # rotateBy a
-- FingerTreeに右から値を格納する演算記号
treeConsr = rotateRegTri 1 (1/12)
-- 左から値を格納する演算記号
treeConsl = rotateRegTri 1 (-1/12)
-- 空集合の記号のパチモン
empty = circle 0.6 <> fromOffsets [1.5 ^& 1.5]  # translateY (-0.8) # translateX (-0.8)
-- 文字記号に見えない長方形を被せたもの
barriortext s xs = text xs # fontSize (local s) <> square s # lw none
-- 変数記号a,等号記号=
symb_a = barriortext 1 "a"
symb_eq = barriortext 1 "="
-- 節点を表す円
node = circle 0.3
-- Single aの記号
single x = node === fromOffsets [0 ^& (-0.6)] === barriortext 1 x
-- Empty |> a = Single a
treeConsr1 = hsep 0.8 [empty , treeConsr , symb_a , symb_eq, single "a" # translateY 0.6]

-- Deep [b] Empty [a]
    -- 木構造を描くための標準的な方法は何だろうか？
node2 = node <> text "2" # fontSize (local 0.3)
node3 = node <> text "3" # fontSize (local 0.3)
