module ProofTree where

import Parts
import Diagrams.Prelude
import DiagramLanguage
import Algebra.Graph hiding(at,(===))
import PGFSurface
import Data.Tree.Lens
import Data.Maybe (fromMaybe)

type B = PGF


-- Int値RoseTreeをオブジェクトのTreeに変換する関数
    -- fが変換の内容
    -- 変換後のオブジェクトにはIntで名前がつく。このIntはIntのTreeを生成するときにAlgaから受け継ぐ。
formulaTree f t = fmap (\n -> named n $ f n) t


onestepDerive args sr ret  =
    case sr of
    []     -> ret
    xs    ->    let ns = names $ mconcat sr
                    n1 = fst $ head ns
                    n2 = fst $ last ns
                    args_ = map alignB args
                    above = hsep 0.2 args_ # centerXY-- 0.2で固定するより、onestepの例のパースが保たれるようサイズに比例させるべき。
                    -- オブジェクト1行分の列の両端の位置を取得し、架線の中央部分を作る
                    s1 = fromMaybe (mkSubdiagram mempty) $ lookupName n1 above
                    s2 = fromMaybe (mkSubdiagram mempty) $ lookupName n2 above
                    p1 = location s1
                    p2 = location s2
                    midp = p1 .+^ 0.5 *^ (p2 .-. p1) -- 両端のsubrootの中間点を確保
                    u = midp .-. origin -- originから中間点へのベクトル。これで架線の中心をsubrootの中心に動かしてみる実験。←これでいい感じになることを確認。
                    l  = norm $ p1 .-. p2
                    -- 架線の両端を調整するためのデータ
                    below = ret # centerXY -- 導出図のroot
                    n = length sr
                    line = if n == 0
                        then mempty 
                        else if n == 1
                            then centerXY $ hrule (1.5*(diameter unitX above))
                            else let r1 = head sr :: Diagram B
                                     r2 = last sr
                                     d1 = diameter unitX r1
                                     d2 = diameter unitX r2
                                 in centerXY $ hrule (1.05*(d1 + l + d2))
                    d = vsep 0.1 $  [above,line # translate u,below # translate u] :: Diagram B
                in d # centerXY
                


proofTree (Node x []) = x
proofTree (Node x ts) = 
    let subRoots = map (view root) ts -- 架線の長さ調節のために深さ1のNode全体を返す
    in onestepDerive (map proofTree ts) subRoots x

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


