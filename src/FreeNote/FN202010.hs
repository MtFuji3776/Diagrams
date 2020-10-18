module FreeNote.FN202010 where

import Parts 
import qualified Algebra.Graph as Alga hiding((===))
import Diagrams.Prelude
import Data.Maybe (fromMaybe)


-- ArrowOptsを先に生成してからconnectOutside'などに渡す仕組みのフローチャートの原案
dia18_1 :: (Show n, Typeable n, RealFloat n) => QDiagram SVG V2 n Any
dia18_1 = 
    ((boxedText "Alga" 1) # named "1" 
    ||| strutX 1 
    ||| (boxedText "Map protoOpts" 1 === boxedText "キー：(Int,Int)" 0.8) # named "2"
    ||| strutX 1
    ||| ((boxedText "Map ArrowOpts" 1 === boxedText "キー：Morph Int" 0.8) # named "3"
    === strutY 2 
    === (boxedText "QDiagram" 1) # named "4"))
    # connectOutside "1" "2" . connectOutside "2" "3" . connectOutside "3" "4"
    <> place (boxedText "辺情報" 0.5) (2^&1) 
    <> place (boxedText "g:Alga" 0.5) (19 ^& (-2)) 
    <> place (boxedText "fmap" 0.5) (12.5 ^& 1)
    
-- Arrowのコンストラクタ関数の従属関係の木
    -- starを使おうとしたらDiagrams.Preludeと衝突したんだが。
dia18_2 = genBCTree 1 $ Alga.path [1,2,3] + 2*4 + Alga.star 3 [5,6,7,8]

-- dia18_3 :: Diagram B
dia18_3 = genLabelTree' (with & slWidth .~ fromMaybe (0,0) . extentX) 1 f (Alga.path [1,2,3,5] + Alga.star 3 [6,7,8] + 2*4)
    where
        f n = 
            case n of 1 -> g "arrow'"
                      2 -> g "arrowAt"
                      3 -> g "arrowBetween'"
                      4 -> g "arrowV'"
                      5 -> g "connect'"
                      6 -> g "connectPerim'"
                      7 -> g "connectOutside'"
                      8 -> g "arrowFromLocatedTrail'"
                      _ -> g ""
        g xs = boxedText xs 0.2
        -- ls =     [
        --             "arrow'"
        --            ,"arrowAt'"
        --            ,"arrowBetween'","arrowV'"
        --            ,"connect'","connectPerim'","connectOutside'","arrowFromLocatedTrail'"
        --          ]
