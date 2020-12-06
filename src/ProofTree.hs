module ProofTree where

import Parts
import Diagrams.Prelude
import DiagramLanguage
import Algebra.Graph hiding(at,(===))
import PGFSurface
import Data.Tree.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Control.Lens.At as Lens (at)

type B = PGF


-- Int値RoseTreeをオブジェクトのTreeに変換する関数
    -- fが変換の内容
    -- 変換後のオブジェクトにはIntで名前がつく。このIntはIntのTreeを生成するときにAlgaから受け継ぐ。
formulaTree f t = fmap (\n -> named n $ f n) t


onestepDerive args sr ret  =
    let ns = names $ mconcat sr
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
            else    let  r1 = head sr :: Diagram B
                         r2 = last sr
                         d1 = diameter unitX r1
                         d2 = diameter unitX r2
                         d3 = diameter unitX ret
                    in if d3 <= 0.5 * d2 + l + 0.5 * d1
                         then lw thin . centerXY $ hrule (0.5 * d1 + l + 0.5 * d2)
                         else lw thin . centerXY $ hrule d3
        d = vsep 0.02 [above,line # translate u,below # translate u] :: Diagram B
    in d # centerXY
            

proofTree (Node x []) = x # alignB # showOrigin
proofTree (Node x ts) = 
    let subRoots = map (view root) ts -- 架線の長さ調節のために深さ1のNode全体を返す
    in onestepDerive (map proofTree ts) subRoots x # alignB # showOrigin

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

test1 = do
    chars <- mapM getPGFObj ["A","B\\land X","C\\to Y","D\\lor (Z \\land W)","E"]
    let alga = 1*(2+3+4) + 4*(5+6+7+8)
        l i = view (ix $ i-1) chars
        t = proofTree . formulaTree l . genTree 1 $ alga
        ds = vsep 0.05 . map (showEnvelope . showOrigin . l) $ [1,2,3,4,5]
        ds1 = showEnvelope . showOrigin . centerXY . hsep 0.2 . map l $ [2,3,4]
    -- num <- getPGFText . show $ diameter unitX t
    return (proofTree . formulaTree (\x -> t) . genTree (1 :: Int) $ alga)

test2 = do
    chars <- mapM getPGFObj ["A","B \\land X", "C \\to Y", "D \\lor (Z \\land W)" , "E"]
    let alga = 1* (2+3) + 2*4 + 3 * 5
        l i = view (ix $ i - 1) chars
        t = proofTree . formulaTree l . genTree 1 $ alga
    return t

proofTree_ (Node x []) = x # alignB
proofTree_ (Node x ts) = hsep 0.1 (map (alignB  . proofTree) ts) === strutY 0.1 === x # centerXY

-- mkMap . namesで、図式中の名前つきSubdiagramの座標のキー配列を構成
    -- キーは名前
    -- ...これ、lookupNameとlocationでやった方が計算効率良いのでは？
mkMap = mconcat . map (Map.fromList . \(x,ys) -> [(x,y) | y <- ys])

lineLength (Node x []) m = (x,0)
lineLength (Node x ts) m = 
    let n1 = head ts
        n2 = last ts
        p1 = fromMaybe origin $ view (Lens.at n1) m
        p2 = fromMaybe origin $ view (Lens.at n2) m
        d  = norm $ p1 .-. p2
    in (x,d) 

-- f = 
--     let t = genTree 1 $ 1*(2+3+4) + 2*(5+6) + 4*(10 + 11 + 12+13 + 14) :: Tree Int
--         d = proofTree_ . formulaTree (flip boxedText 0.1 . show) $ t :: Diagram PGF
--         getSub :: Int -> Subdiagram PGF V2 Double Any
--         getSub n = fromMaybe (mkSubdiagram mempty) $ lookupName n d
--         getPoint = location . getSub
--         t' = fmap (\n -> (boxedText (show n) 0.1 ,getPoint n)) t
--         h dt = case dt of Node x [] -> fst x
--                           Node x [t] -> 
--                             let d' = h t :: Diagram PGF
--                                 dia = diameter unitX $ d' :: Double
--                                 l = hrule dia # centerXY # lw thin :: Diagram PGF
--                             in d' === l === x
--                           Node x ts -> 
--                               let p1  = snd (view root $ head ts)
--                                   p2  = snd (view root $ last ts)
--                                   dis = norm $ p1 .-. p2
--                                   l = hrule dis # lw thin # centerXY
--                               in (hsep 0.1 $ map h ts) # alignB === strutY 0.02 === l === strutY 0.02 === fst x # centerXY
--     in h t'
        