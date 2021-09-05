{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}
module ProofTree where

import Parts
import Diagrams.Prelude hiding(star)
import DiagramLanguage
import Algebra.Graph hiding(at,(===))
import qualified Algebra.Graph.AdjacencyMap as Adjacency
import PGFSurface
import Data.Tree.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Lens.At as Lens (at)
import Text.RawString.QQ

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
            

proofTree (Node x []) = x # alignB
proofTree (Node x ts) = 
    let subRoots = map (view root) ts -- 架線の長さ調節のために深さ1のNode全体を返す
    in onestepDerive (map proofTree ts) subRoots x # alignB

derivTree f r = proofTree . formulaTree f . genTree r

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

test3 = do
    objs <- mapM getPGFObj ["A","B","C","D","E","F","G","H"]
    let alga = path [1,3,4,5,6] + 1*2
        tr = genTree 1 alga
        slopt = def & slWidth  .~ fromMaybe (0,0) . extentX
                    & slHeight .~ fromMaybe (0,0) . extentY
                    & slHSep   .~ 0.15 
                    & slVSep   .~ 0.02
        f i = view (ix $ i - 1) objs
        sltree = symmLayout' slopt $ fmap f tr
    return $ renderTree id (\_ _ -> mempty) sltree

-- 導出図用のSymmLayoutOption
ptLayoutOpts = def & slWidth  .~ fromMaybe (0,0) . extentX
                   & slHeight .~ fromMaybe (0,0) . extentY
                   & slHSep   .~ 0.2
                   & slVSep   .~ 0.04

-- 導出図用のrenderTree
renderPt = renderTree id (\_ _ -> mempty)

putGhost (Node x []) = 
    let w = width x
        h = height x
    in Node x [Node (rect w h # lw none) []]
putGhost (Node x ts) = Node x (map putGhost ts)


boxingNodes t = let 
    subts = subForest t
    ds = map (view root) subts
    d = hsep 0.2 ds
    l = 1.2 * width d
    t' = over root (atop (strutX l # centerXY)) t
    in over branches (map boxingNodes) t'


-- symmLayout'に導出図用のレイアウトオプションを部分適用したもの
    -- これを生成するときに、導出図ノードの初期値を生成できないだろうか？
    -- コンストラクタを引数に取る高階関数にしとくのが無難っぽい。柔軟性もこのほうが高そう
ptLayout f = symmLayout' ptLayoutOpts . boxingNodes . fmap f

-- zipのRoseTree版の感じで、DiagramツリーとPointツリーからタプルツリーを生成する
-- zipTree :: Tree (Diagram B) -> Tree (P2 Double) -> Tree (Diagram B,P2 Double)
zipTrees (Node x []) (Node p subf) = Node (x , p) []
zipTrees (Node x subf) (Node p []) = Node (x,p) []
zipTrees (Node x subf1) (Node p subf2) = Node (x , p) (zipWith zipTrees subf1 subf2)

-- boxingNodesを交えて計算した座標を、box無しのオブジェクトに付与する関数
modifiedLayout f nt = let 
    -- boxingNodesでレイアウトをとって、座標だけ取得する
    dpt = ptLayout f nt
    pt = fmap snd dpt
    -- boxingなしのオブジェクトツリーを作る
    dt = fmap f nt
    -- boxingなしのオブジェクトツリーに座標を付与
    in zipTrees dt pt



test4 = do
    objs <- mapM getPGFObj ["A","B","CSTUVWXYZ","D","E","F","G"]
    let alga = path [1,3,4,5,6] + 1*2
        tr = genTree 1 alga
        f i = view (ix $ i - 1) objs
        sltree = ptLayout (evalPTNode . fromInt f) tr
    return $ renderPt sltree

data PTNode a = PTNode{elementPTN :: a
                      ,leftLab :: a
                      ,rightLab :: a
                      ,linePTN :: a} deriving(Typeable)

instance Monoid a => Default (PTNode a) where
    def = PTNode mempty mempty mempty mempty

-- genTreeとAlgaから生成したTree Intを、PTNodeで初期化する。その際、getPGFObjと併用することを見越して、DiagramをIntから指定するコールバック関数fを引数に取らせる。
    -- ptLayout (fromIntTree f)という形式で使用できるはずだ。
    -- あ、symmLayout'を使うなら先にPTNodeにしちゃダメかも？
    -- ↑ダメだろ。ついでにその場でevalしちゃうのも愚策。PTNodeにしたのは後でデータを活用するために他ならない。
fromInt f n = def{elementPTN = f n}

fromDiagram d = def{elementPTN = d}

evalPTNode (PTNode d ll rl line) = mconcat [d,ll ||| line ||| rl]

test5 = do
    objs <- mapM getPGFObj ["A","B","C","D","E","F"]
    let f i = view (ix $ i-1) objs
        alga = path [1,2,3,4,5] + path [2,6,7]
        t = genTree 1 alga
        t' = fmap (fromDiagram . uncurry place) $ ptLayout f t
        ds = flatten t'
        ks = flatten t
        kds = zip ks ds
        mp = Map.fromList kds
    return mp

evalNodeMap = foldr mappend mempty


-- 改良すべき関数。部分導出木の１段が横に長いときに、下の無関係な式にも架線がかかってしまう不具合。
makeMap objs t =
    let f i = view (ix $ i-1) objs
        ds = map (uncurry place) . flatten . modifiedLayout f $ t
        ks = flatten t
        kds = zip ks ds
        mp = Map.fromList kds
    in mp



-- onestepPair t mp =
--     let g = Adjacency.tree t
--         ks = flatten t
--         kks = map (\x -> (x,Adjacency.postSet x g)) ks
--         rmMaybe = fromMaybe mempty
--         ds = map (\(x,ys) -> (rmMaybe $ view (Lens.at x) mp , map (\z -> rmMaybe.view (Lens.at z)) . Set.toList) ys) kks
--     in ds

onestepLine d [] = mempty
onestepLine d ds =
    let w = width d
        w' = width . mconcat $ ds
        d0 = if w >= w' then d else mconcat ds
        y0 = -0.02 + (fst . fromMaybe (0,0) $ extentY d)
        (x1,x2) = fromMaybe (0,0) . extentX $ d0
    in (x1 ^& y0) ~~ (x2 ^& y0) :: Diagram PGF

test6 = do
    objs <- mapM getPGFObj ["A","B","C","D","E"]
    let alga = 1*(2+3+4+5)
        f i = view (ix $ i - 1) $ scaleY (-1) objs
        t = genTree 1 alga
        t' = fmap (uncurry place) $ ptLayout f t
        d = rootLabel t'
        ds = concatMap flatten . subForest $ t'
    return $ scaleY (-1) $ centerXY $ d <> mconcat ds <> onestepLine d ds


select mp n = fromMaybe mempty . view (Lens.at n) $ mp

onestepObjects mp = over _1 (select mp) . over _2 (map (select mp))

test7 = do
    objs <- fmap (scaleY (-1)) $ mapM getPGFObj ["A","B","C","D","E","F"] :: OnlineTex [Diagram PGF]
    let alga = 1*(2+3) + 2*(4+5) + 3*6
        t = genTree 1 alga
        mp = makeMap objs t
        ks = flatten t
        adjag = Adjacency.tree t
        derivs = map (onestepObjects mp) $ makeRelations adjag ks
        ls = map (uncurry onestepLine) derivs
    return $ scaleY (-1) . centerXY $ mconcat ls <> evalNodeMap mp

onestepRelation g n =
    let ks = Set.toList $ Adjacency.postSet n g
    in (n,ks)

makeRelations g ns = map (onestepRelation g) ns

-- オブジェクトリストとTree Intから導出図を生成する
    -- ひとまず完成したが、後で改良すべき関数。mpをDiagram PGFではなくPTNodeマップにして、ラベル付けや架線の装飾を出来るようにしたい
    -- それとlsをPTNodeマップに統合して、架線の装飾をマップ上で済ませられるようにしたい。引数にupdate関数を取るよう後々設計し直す。


data DerivingLine a = DLine{_derivingLine :: a
                            ,_leftLabel :: a
                            ,_rightLabel :: a}deriving(Typeable)

$(makeLenses ''DerivingLine)

instance Monoid a => Default (DerivingLine a) where
    def = DLine mempty mempty mempty

evalDLine (DLine dl ll rl) = 
    let (l,r) = fromMaybe (0,0) $ extentX dl
        y = (uncurry (+) $ fromMaybe (0,0) $ extentY dl) / 2
        rl' = place (rl # alignL) (r ^& y)
        ll' = place (ll # alignR :: Diagram PGF) (l ^& y)
        dl' = mconcat [dl,rl',ll']
    in dl'

genProofTree update objs t =
    let objs' = map (scaleY (-1)) objs
        mp = makeMap objs' t
        adjag = Adjacency.tree t
        ks = flatten t
        kss = makeRelations adjag ks
        derivs = map (onestepObjects mp) kss
        initLine l = def & derivingLine .~ l
        ls = map (initLine . uncurry onestepLine) derivs
        mpDLine' = Map.fromList . zip ks $ ls
        mpDLine = update mpDLine'
        d = evalNodeMap mp <> foldr mappend mempty (fmap evalDLine mpDLine)
    in scaleY (-1) $ centerXY  d :: Diagram PGF

genProofTree_ = genProofTree id

test8 = do
    objs <- mapM getPGFObj ["A\\land A \\Rightarrow B \\to B","B","C","D","E","F","G"] :: OnlineTex [Diagram PGF]
    let alga = path [1,2,4] + 2*5 + path [1,3,6]
        t = genTree 1 alga
        d = genProofTree_ objs t
    return d

test9 = do
    let alga = path [1,2,3,4,5,6,7,8] + path [2,9,10,11] + path [2,12,13,14,15] + path [4,16,17] + path [9,18,19]
        t = genTree 1 alga
    objs <- mapM (getPGFObj.show) $ flatten t
    let d = genProofTree_ objs t
    return d


getFormula = mapM (fmap alignB . getPGFObj . ("\\mathstrut " <>))

test10 = do
    objs <- mapM (fmap alignB . getPGFObj . ("\\mathstrut " <>)) ["(z \\Rightarrow x) \\to (z \\Rightarrow x)","z \\land (z \\Rightarrow x) \\to x","x \\to y","z \\land (z \\Rightarrow x) \\to y","z \\Rightarrow x \\to z \\Rightarrow y"]
    let alga = path [5,4,2,1] + 4*3
        d = genProofTree_ objs $ genTree 5 alga
    return d

test11 = do
    objs <- getFormula ["z \\Rightarrow x \\rightharpoonup z \\Rightarrow y","z \\land (z \\Rightarrow x) \\rightharpoonup x","x \\rightharpoonup y","z \\land (z \\Rightarrow x) \\rightharpoonup y","z \\Rightarrow x \\rightharpoonup z \\Rightarrow y"]
    let alga = path [5,4,2,1] + 4*3
        d = genProofTree_ objs $ genTree 5 alga
    return d

test12 = do
    objs <- getFormula ["z \\Rightarrow x \\rightharpoonup z \\Rightarrow y","z \\land (z \\Rightarrow x) \\rightharpoonup x","x \\rightharpoonup y","z \\land (z \\Rightarrow x) \\rightharpoonup y","z \\Rightarrow x \\rightharpoonup z \\Rightarrow y"] :: OnlineTex [Diagram PGF]
    lab1 <- fmap (scale 0.6 . scaleY (-1)) $ getPGFLabel "[\\Rightarrow]"
    lab2 <- fmap (scale 0.6 . scaleY (-1)) $ getPGFLabel "[\\rightharpoonup \\rightharpoonup]"
    lab3 <- fmap (scale 0.6 . scaleY (-1)) $ getPGFLabel "[\\Rightarrow]"
    let alga = path [5,4,2,1] + 4*3
        update mp = over (Lens.at 5) (fmap $ set rightLabel lab1)  . over (Lens.at 2) (fmap $ set rightLabel lab3) . over (Lens.at 4) (fmap $ set rightLabel lab2) $ mp 
        d = genProofTree update objs (genTree 5 alga)
    return d

oneSequent sym xs ys = between xs ys sym

(|-) = oneSequent " \\vdash "

(-->) = oneSequent " \\rightharpoonup "

test13 = do
    objs <- getFormula ["\\exists x (B \\land A)" --> "\\exists x A \\land B"
                        ,"\\exists x (B \\land A)" --> "B \\land \\exists x A"
                        ,"B \\land A" --> "|x| \\land \\exists x A \\land B"
                        ,"B \\land A" --> "|x| \\land \\exists x A"
                        ,"B \\land A" --> "B"
                        ,"A" --> "|x| \\land \\exists x A"
                        ,"\\exists x A" --> "\\exists x A"
                        ,[r|B \land A|] |- [r|B \land A|] 
                        ,"B \\land A" --> "A" 
                        ]
    let alga = path [1,2,3,4,6,7] + 4*(8) + 3*9 
        d = genProofTree id objs (genTree 1 alga)
    return d

test14 = do
    objs <- getFormula $ map show [1,2,3,4,5,6,7,8,9,10,11]
    let alga = path [1,3,5,7] + path [1,2,8] + path [1,4,6,9] + star 5 [10,11]
        d = genProofTree id objs (genTree 1 alga)
    return d

-- boxingNodes t = let 
--     subts = subForest t
--     ds = map (view root) subts
--     d = hsep 0.5 ds
--     l = width d
--     t' = over root (atop (strutX l)) t
--     in over branches (map boxingNodes) t'

test15 = do
    objs <- getFormula $ [
            [r|\forall x (B \land A)|] --> [r|B \land \forall x A|],
            [r|\forall x (B \land A)|] --> "\\forall x A",
            [r|\forall x (B \land A) \land |x||] --> "A",
            [r|\forall x (B \land A) \land |x||] --> "B \\land A",
            [r|\forall x (B \land A)|] --> [r|\forall x (B \land A)|],
            [r|B \land A|] --> "A",
            [r|\forall x (B \land A)|] --> "B",
            [r|\forall x (B \land A)|] --> [r|\forall x B|],
            [r|B \land A|] --> "B",
            [r|\forall x B|] --> "B",
            [r|\forall x B|] --> [r|B \land |x||],
            "B \\land |x|" --> "B",
            [r|\forall x B \land |x||] --> "B",
            [r|\forall x B|] --> [r|\forall x B\text{test test test test test test test test test test}|]
            ]
    let alga = path [1,2,3,4,5] + 3*6 + path [1,7,10,11,13,14] + path [7,8,9] + 10 * 12
        t = genProofTree id objs (genTree 1 alga)
    return t

test16 = do
    let sequent = ("\\Gamma" |-)
        alga = 1*(2+3)
    objs <- getFormula $ map sequent ["\\varphi \\wedge \\psi", "\\varphi", "\\psi"]
    return $ genProofTree id objs  (genTree 1 alga)

