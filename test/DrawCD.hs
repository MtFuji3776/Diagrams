{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts, TypeFamilies #-}
module DrawCD where

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
import Guide_Arrows(renderTest)
import qualified Graphics.SVGFonts as SF


obj = circle 0.02 # fc black -- :: (TrailLike b, Transformable b, Typeable (N b) , HasStyle b, V b )

sqr = fromOffsets [unitX,unitY,-unitX,-unitY]

coor x y d = d # translateX x # translateY y

example1 = sqr <> mconcat (map (uncurry coor) [(0,0),(1,0),(1,1),(0,1)] <*> [obj]) <> square 1 -- :: Diagram B

easyRenderReact name = renderPretty ("/Users/fujimotomakoto/PureScriptProjects/react/tutorial/src/img/" <> name) fixedSize


-- 次の二つをリファクタリングせよ
-- tridia_CharSymb = connectOutside 3 1 . connectOutside 3 2 . connectOutside 1 2 $ pad 1.3 $ atPoints (triangle 1 # scaleY(-1)) $ zipWith named [1..] $ map (((\t -> t <> square 0.2 # lw 0).fontSize (local 0.15)) . text) ["A'" , "B", "A"]
-- tridia_CircSymb = connectOutside 3 1 . connectOutside 3 2 . connectOutside 1 2 $ pad 1.3 $ atPoints (triangle 1 # scaleY(-1)) $ zipWith named [1..] $ repeat $ circle 0.05 # fc black

-- 文字列記号の生成
genCharSymbs s xs =
    let defSize       = fontSize . local
        laySquare s t = t <> square (s * 1.5) # lw 0
    in map (laySquare s . defSize s . text) xs

-- 黒円の生成
genBlCirc s = repeat $ circle s # fc black

-- 記号リストに名前をつける
genNamedSymbs = zipWith named

-- お手軽な名付け関数
    -- 記号リストが無限のときに注意が必要か？
intNamedSymbs = genNamedSymbs ([1..] :: [Int])

-- -- 文字列記号をatPointsでDiagram化
-- getObjs_Symb tl names s xs = 
--     let objs = genNamedSymbs names $ genCharSymbs (0.15*s) xs
--     in atPoints tl objs # pad 1.3

-- -- 黒円記号をatPointsでDiagram化
-- getObjs_Circ tl names s = 
--     let objs = genNamedSymbs names $ genBlCirc (0.05 * s)
--     in atPoints tl objs # pad 1.3
-- これら二つまとめて抽象化した方が良さげ

genNamedObjs tl names objs = 
    let objs' = genNamedSymbs names objs
    in atPoints tl objs' # pad 1.3

-- TrailLikeから対象だけの離散圏図式を生成する関数
    -- sはスケールで、図式中の射の長さの最小値。まずは手動で定義したが、図式から導出させて自動化したい
genObjs_Char tl names s xs = genNamedObjs tl names $ genCharSymbs (s*0.15) xs

-- PGFならばtextを正しく輪郭取ってくれると期待してみたが無意味だった
--genObjs_CharPGF tl names s xs = genNamedObjs tl names $ map (fontSize (local s) . text) xs

genObjs_Circ tl names s = genNamedObjs tl names $ genBlCirc (s * 0.05)