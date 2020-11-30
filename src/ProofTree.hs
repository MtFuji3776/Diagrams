module ProofTree where

import Parts
import Diagrams.Prelude
import DiagramLanguage
import Algebra.Graph hiding(at,(===))
import PGFSurface
import Data.Tree.Lens
import Data.Maybe (fromMaybe)

type B = PGF


onestepDerive args ret =
    let n = length args
        args_ = over (ix 0) (named "1") . over (ix $ n-1) (named "2") $ map alignB args
        above = hsep 0.2 args_ -- 0.2で固定するより、onestepの例のパースが保たれるようサイズに比例させるべき。
        -- オブジェクト1行分の列の両端の位置を取得し、架線の中央部分を作る
        s1 = fromMaybe (mkSubdiagram mempty) $ lookupName "1" above
        s2 = fromMaybe (mkSubdiagram mempty) $ lookupName "2" above
        p1 = location s1
        p2 = location s2
        l  = norm $ p1 .-. p2
        -- 架線の両端を調整するためのデータ
        d1 = diameter unitX s1 -- 本当はsubtreeのrootから取得すべきかもしれない直径。args_を作る前にheadとlastだけ取得しとけば可能なはず。
        d2 = diameter unitX s2 -- こちらも同様
        below = ret # centerXY -- 導出図のroot
        -- width = diameter unitX above
        line = if n <= 1 then centerXY $ hrule (1.5*d1)
                         else centerXY $ hrule (0.6*d1 + l + 0.6*d2)
        d = vsep 0.1 [above # centerXY,line,below] :: Diagram B
    in d


proofTree (Node x []) = x
proofTree (Node x ts) = 
    -- let mainPoints = map (fromMaybe origin . rayTraceP origin unit_Y) ts
    --     aboveObjs = map (view root) ts 
    --     objsOnTheEdge = [head aboveObjs,last aboveObjs]
    --     width = diameter unitX above
    --     line = hrule (1.2 * width) # centerXY
    onestepDerive (map proofTree ts) x

test = do
    chars <- mapM getPGFObj ["A","B","C","D","E"]
    let objs = hsep 0.2 chars # centerXY :: Diagram B
        objs' = objs === objs === (view (ix 3)chars)
        width = diameter unitX objs'
        height = diameter unitY objs'
        feet = objs' # clipTo (rect width (0.2*height) # alignB)
        width1 = diameter unitX feet
        line = hrule width1 # centerXY
    return $ (objs'  === line === (view (ix 0) chars))


