module FreeNote.FN202011 where

import Parts
import DiagramLanguage
import Diagrams.Prelude
import qualified Algebra.Graph as Alga hiding ((===))
import Data.Maybe (fromMaybe,isNothing)
import Diagrams.TwoD.Arrow
import Diagrams.BoundingBox
import qualified Control.Lens as Lens (at,(?~))
import qualified Data.Map as Map


-- 引き戻しの図式を回転させてみた
    -- あとgenDiagramの試運転
dia1_1 =
    let rot = rotateBy (-1/8)
        loctrl = rot $ fromOffsets [unitX,unitY,unit_X,0.5*^(unit_X + unitY)]
        labels = map (flip boxedText 0.15) ["A","C","B","P","X"]
        alga   = 5*(1+4+3) + 4*(1+3) + (1+3)*2
        plbSymbol = plb # translateY 0.75 # rot
    in genDiagram loctrl labels alga <> plbSymbol


-- MorphOptsをLensで編集する練習
dia2_1 =
    let loctrl = fromOffsets [unitX + 0.5 *^ unitY] :: Trail V2 Double
        lab = attachLabel (loctrl `at` origin) (boxedText "f" 0.15 :: Diagram B) 0.5
        mopts = (def :: MorphOpts) & set locTrail ((monicShaft  loctrl) `at` origin)
                                   & set arrOpts openHead
                                   & over symbols (lab <|)
        evalMopts opts = 
            let trl = view locTrail opts
                arropts = view arrOpts opts
                symbs = view symbols opts
            in arrowFromLocatedTrail' arropts trl <> mconcat symbs
    in evalMopts mopts <> square 1 # lw none # scale 0.01

-- 良い出来なので公式採用。DiagramLanguageに移行（コピペ）
-- evalMorphOpts opts =
--     let trl     = opts ^. locTrail
--         arropts = opts ^. arrOpts
--         symbs   = opts ^. symbols
--     in arrowFromLocatedTrail' arropts trl <> mconcat symbs

_monic (Morph loctrl opts symbs) = 
    let p1 = atParam loctrl 0 
    in over locTrail (flip at p1 . monicShaft . unLoc) (Morph loctrl opts symbs)

-- LocatedTrailを一旦Trailに戻してから、道の結合演算などを施して、再びLocatedTrailに戻す
    -- 戻り値の始点は変更しない仕様
    -- 矢印のShaftを変形させるときに使うと良さそう（monic以外にarc系でも頻繁に使うはず）
-- buildLocTrail someFuncOnTrail loctrl =
--     let p0 = atParam loctrl 0
--     in flip at p0 . someFuncOnTrail . unLoc $ loctrl

dia3_1 =
    let mopts = def & locTrail .~ fromOffsets [unitX]
        trl = mopts ^. locTrail
        deco mopts = mopts & locTrail %~ buildLocTrail monicShaft
                           & symbols %~ ((attachLabel trl (boxedText "X" 0.15) 0.5 :) . (attachLabel trl reticle 0.3 :))
    in evalMorphOpts $ deco mopts

-- 文字を含む図のお試し
dia3_2 =
    let loctrl = fromOffsets [unitY,unitY,unitY,unit_Y + 4 *^ unitX , 2*^ unit_Y]
        objs   = map (lw none . flip box 0.05) 
                $ map (flip boxedText 0.15) 
                    [
                        "[((Int,Int),MorphOpts)]",
                        "Alga",
                        "[QDiagram]",
                        "LocatedTrail"
                    ] 
                    ++
                    [ 
                        (stroke $ textSVG "(QDiagram,Map (Int,Int) MorphOpts)" 0.30) # lw none # fc red
                    ] 
                    ++
                    [
                        boxedText "QDiagram" 0.15
                    ]
        alga = (1+2+3+4)*5 + 5*6
    in genDiagram loctrl objs alga

dia3_2' =
    let loctrl = fromOffsets [unitY,unitY,unitY,unit_Y + 4 *^ unitX , 2*^ unit_Y]
        objs   = map (lw none . flip box 0.05) 
                $ map (fc blue . lw none . stroke . flip textSVG 0.30) 
                    [
                        "[((Int,Int),MorphOpts)]",
                        "Alga",
                        "[QDiagram]",
                        "LocatedTrail",
                        "(QDiagram,Map (Int,Int) MorphOpts)",
                        "QDiagram"
                    ]
        alga = (1+2+3+4)*5 + 5*6
    in genDiagram loctrl objs alga

-- 図式言語のお試し
    -- genGraphLocTrailの仕様に難あり。[LocatedTrail]と[Diagram B]でatPointsするのではAlgaで制御できなくなる
dia3_3 =
    let f alga =
            let loctrl = fromOffsets [unitY,unit_Y + unitX]
                objs = map (lw none . flip box 0.02 . fc black . strokeP . flip textSVG 0.2) ["M","A","B"]
                --alga = 2*(1+3) + 1*3
                (disd,mp) = genGraphLocTrail loctrl objs alga
                mp' = over (Lens.at (uncurry Single (1,3))) (fmap monic) mp
            in mkDiagram (disd,mp')
    in f (1*3) ||| vline 1.7 Forall ||| f ((1+2)*3) ||| vline 1.7 Only ||| f ((1+2)*3 + 2*1)

dia3_3' =
    let f alga =
            let loctrl = fromOffsets [unitY,unit_Y + unitX]
                objs = replicate 3 bc
                (disd,mp) = genGraphLocTrail loctrl objs alga
                mp' = fmap (\opts -> set (arrOpts . headLength) (local 0.06) . set (arrOpts . gaps) (local 0.02) $ opts)   $ over (Lens.at (Single 1 3)) (fmap monic) mp
            in mkDiagram (disd,mp')
    in f (1*3) ||| vline 1.7 Forall ||| f ((1+2)*3) ||| vline 1.7 Only ||| f ((1+2)*3 + 2*1)

dia3_4 = 
    let loctrl = fromOffsets [unitY,unitY,unit_Y + 2 *^ unitX, 1.1 *^ unitX + unitY, unit_Y,0.5 *^ unit_X , 2*^ unit_Y ]
        objs = map (lw none . flip box 0.05) 
             $ map (lw none. flip box 0.02 . fc blue . strokeP . flip textSVG 0.2) 
            [
                "Alga",
                "[QDiagram]",
                "Located Trail",
                "(QDiagram,",
                "[((Int,Int),MorphOpts)]",
                "Map (Int,Int) MorphOpts)",
                "",
                "QDiagram"
            ]
        alga = (1+2+3)*4 + 5*6 + 7*8
    in genDiagram loctrl objs alga

dia4_1 =
    let trl = fromOffsets [unitY,unit_Y + unitX]
        objs = replicate 3 bc
        alga1 = 1*3
        alga2 = (1+2)*3
        alga3 = (1+2)*3 + 2*1
        (disd,opmap) = genGraphLocTrail trl objs alga2
        pmap' xmap = xmap & Lens.at (Single 1 3) %~ fmap equalizer
        opmap' = pmap' opmap
    in mkDiagram (disd,opmap')

dia4_2 =
    let trl = fromOffsets [unitY,unit_Y + unitX]
        objs = replicate 3 bc
        alga1 = 1*3
        alga2 = (1+2)*3
        alga3 = (1+2)*3 + 2*1
        --(disd,opmap) = genGraphLocTrail trl objs alga2
        diagrams = map (genGraphLocTrail trl objs) [alga1,alga2,alga3]
        pmap' xmap = xmap & Lens.at (Single 1 3) %~ fmap equalizer
        diagrams' = map (over _2 pmap') diagrams
    in foldr (\x y -> x ||| vline 1.7 Forall ||| y) mempty $ map mkDiagram diagrams'

-- 図式言語の量化記号訂正版
dia4_2' =
    let trl = fromOffsets [unitY,unit_Y + unitX,unitX]
        objs = replicate 4 bc
        alga1 = 1*3 + 4
        alga2 = (1+2)*3 + 4
        alga3 = (1+2)*3 + 2*1 + 4
        diagrams = map (genGraphLocTrail trl objs) [alga1,alga2,alga3]
        pmap' xmap = xmap & Lens.at (Single 1 3) %~ fmap equalizer
                          -- & Lens.at (Twin 3 4 True) . locTrail %~ fmap (translate )
        mktwinarr d =
            let tpl = (3,4) :: (Int,Int)
                trl = mkLocTrail tpl d
                u   = 0.1 *^ (normalAtParam trl 0)
                trl1 = trl # translate u
                trl2 = trl # translate (-u)
                p = atParam trl 0.5
                reti = place reticle p
            in arrowFromLocatedTrail trl1 <> arrowFromLocatedTrail trl2 <> reti <> d
        diagrams' = map (mkDiagram . over _1 mktwinarr . over _2 pmap') diagrams
        height = foldr max 0 . map heightOfVline $ diagrams'
        vlines = verticals height [NoLine,Forall,ExistsOnly]
    in foldr (|||) mempty $ zipWith (|||) vlines diagrams'

-- 図式言語手習い
    -- 「任意のcoverはepiである」という主張
dia4_3 =
    let trl = fromOffsets [unitY,unit_Y + unitX]
        objs = replicate 3 bc
        alga1 = 2*1
        alga2 = 2*(1+3)
        alga3 = 2*(1+3) + 1*3
        diagrams = map (genGraphLocTrail trl objs) [alga1,alga2,alga3]
        cover opts = opts & arrOpts . headStyle %~ fc white . lw 0.7
        setting xmap = xmap & Lens.at (Single 2 1) %~ fmap cover
        diagrams' = map (mkDiagram . over _2 setting) diagrams
        height = foldr max 0 . map heightOfVline $ diagrams'
        vlines = verticals height [Forall,Forall,Only]
    in foldr (|||) mempty $ zipWith (|||) vlines diagrams'

-- 図式言語手習い「coverの定義」
    -- 未完成。Twinの自動化と、平行で逆向きな二つの射を描くための関数を用意しなければならない
dia4_4 =
    let trl = fromOffsets [unitX + unitY, unit_Y]
        objs = replicate 3 bc
        alga1 = 2*3
        alga2 = 2*(1+3) + 1*3
        alga3 = alga2 + 3*1
        protoDia = map (genGraphLocTrail trl objs) [alga1,alga2,alga3]
        cover = over (arrOpts . headStyle) (fc white . lw 0.8)
        setting = over (Lens.at (Single 2 3)) (fmap cover)
        protoDia' = map (mkDiagram . over _2 setting) protoDia
        height = foldr max 0 . map heightOfVline $ protoDia'
        vlines = verticals height [NoLine,Forall,Exists]
    in foldr (|||) mempty $ zipWith (|||) vlines protoDia'

-- 量化子リストと図式リストを受け取って図式言語を構成する関数
    -- 実用性抜群なのでDiagramLanguage.hs行き
-- diagramLanguage qs ds =
--     let height = foldr max 0 . map heightOfVline $ ds
--         vlines = verticals height qs
--     in foldr (|||) mempty $ zipWith (|||) vlines ds

-- diagramLanguageを使えば更にコードが短くなる
dia4_4' =
    let trl = fromOffsets [1.5 *^ (unitX + unitY), 1.5 *^ unit_Y]
        objs = replicate 3 bc
        alga1 = 2*3
        alga2 = 2*(1+3) + 1*3
        alga3 = alga2 
        protoDia = over (ix 2 . _2) (twin 1 3 0.05) $ map (genGraphLocTrail trl objs) [alga1,alga2,alga3]
        cover = over (arrOpts . headStyle) (fc white . lw 0.9)
        setting = over (Lens.at (Single 2 3)) (fmap cover) 
                . over (Lens.at (Twin 1 3 False)) (fmap monic)
                . over (Lens.at (Single 1 3)) (fmap monic)
        protoDia' = map (mkDiagram . over _2 setting) protoDia
    in diagramLanguage [NoLine,Forall,Exists] protoDia'


-- 平行射を生成してLocatedTrailを動かす関数の試作品
    -- 指定したキーの値が存在しなければ何もしない
-- twin i j n xmap = 
--     let e = view (Lens.at (Single i j)) xmap
--         setTwinKeys = set (Lens.at (Twin i j True)) e . set (Lens.at (Twin i j False)) e . sans (Single i j)
--         movetrl b opts = 
--             let trl = view locTrail opts
--                 u = n *^ normalAtParam trl 0
--                 trans   = if b then translate u . reverseLocTrail
--                                else translate (-u) 
--             in over locTrail trans opts
--         lens_Map b = over (Lens.at (Twin i j b)) (fmap (movetrl b))
--         xmap' = lens_Map True . lens_Map False . setTwinKeys $ xmap
--     in if isNothing e 
--         then xmap
--         else xmap'

dia5_1 = 
    let trl = fromOffsets [unit_X + unitY , unitX]
        objs = map (lw none . flip Parts.box 0.02 . fc black . strokeP . flip textSVG 0.15) ["P","P","Q"]
        alga = 2*(1+3) + 3*1
        protoDia = genGraphLocTrail trl objs alga
        lab i j x = 
            let opts = view (Lens.at (Single i j)) $ view _2 protoDia
                trl  = view locTrail $ fromMaybe def opts
            in over symbols (((attachLabel trl (textSVG x 0.14 # strokeP # fc black # lw none) 0.5))<|)
        setting mp = mp & (Lens.at (Single 2 1)) %~ fmap (lab 2 1 "1")
                        & (Lens.at (Single 2 3)) %~ fmap (lab 2 3 "s")
                        & (Lens.at (Single 3 1)) %~ fmap (lab 3 1 "r")
        protoDia' = mkDiagram . over _2 setting $ protoDia
    in protoDia'

dia5_1' =
    let trl = fromOffsets [unit_X + unitY , unitX]
        objs = map (lw none . flip Parts.box 0.02 . fc black . strokeP . flip textSVG 0.15) ["P","P","Q"]
        alga = 2*(1+3) + 3*1
        protoDia = genGraphLocTrail trl objs alga
        lab i j x = 
            let opts = view (Lens.at (Single i j)) $ view _2 protoDia
                trl  = view locTrail $ fromMaybe def opts
            in over symbols (((attachLabel trl (text x # fontSize (local 0.08)) 0.5))<|)
        setting mp = mp & (Lens.at (Single 2 1)) %~ fmap (lab 2 1 "1")
                        & (Lens.at (Single 2 3)) %~ fmap (lab 2 3 "s")
                        & (Lens.at (Single 3 1)) %~ fmap (lab 3 1 "r")
        protoDia' = mkDiagram . over _2 setting $ protoDia
    in protoDia'

-- dia5_1から抽出した、対象記号用の文字列関数 
    -- AMSMathのフォントを使えるようにしたい
-- svgObject = lw none . flip Parts.box 0.01 . fc black . strokeP . flip textSVG 0.15
-- dia5_1から抽出した、ラベル記号用の文字列関数
-- svgLabel = lw none . fc black . strokeP . flip textSVG 0.14
-- いずれもDiagramLanguage行き
    -- Partsとどっちが良いだろうか？

-- MorphOptsを受け取り、LocTrailから情報を取り出して、attachLabelを適用
    -- DiagramLanguage行き
-- takeLabel x p opts = 
--     let trl = view locTrail opts
--         lab = attachLabel trl x p
--     in over symbols (lab <|) opts
-- すでにそっくりなのを作っといて忘れていた…


dia5_1'' =
    let trl = fromOffsets [unit_X + unitY , unitX]
        objs = map svgObject ["P","P","Q"]
        alga = 2*(1+3) + 3*1
        protoDia = genGraphLocTrail trl objs alga
        l_1 = svgLabel "1"
        l_s = svgLabel "s"
        l_r = svgLabel "r"
        setting mp = mp & Lens.at (Single 2 1) %~ fmap (takeLabel l_1 0.5 False) -- ここらへん抽象化できそう
                        & Lens.at (Single 2 3) %~ fmap (takeLabel l_s 0.5 True)  -- Lens.at キー %~ fmap f のパターン
                        & Lens.at (Single 3 1) %~ fmap (takeLabel l_r 0.5 True)  -- 記述がさらに短くなる予感
    in mkDiagram . over _2 setting $ protoDia

-- キーと作用関数を受け取ると、Mapにアクセスしてキーのところの要素に作用を掛ける関数
    -- 思いの外綺麗にまとまったのでDiagramLanguage行き
    -- 二項演算っぽく書いても良いかも？attachment感のある演算記号ってどんなだろう。
actInMap key act = over (Lens.at key) (fmap act)

-- 意味不明のうざいエラーが頻出するので一旦中断。
-- dia5_1''' = 
--     let trl = fromOffsets [unit_X + unitY , unitX]
--         objs = map svgObject ["P","P","Q"]
--         alga = 2*(1+3) + 3*1
--         protoDia = genGraphLocTrail trl objs alga
--         l_1 = takeLabel (svgLabel "1") 0.5 False
--         l_s = takeLabel (svgLabel "s") 0.5 True
--         l_r = takeLabel (svgLabel "r") 0.5 True
--         setting = actInMap (Single 2 1) l_1 . actInMap (Single 2 3) l_s . actInMap (Single 3 1)
--         mp = setting $ snd protoDia
--     in mkDiagram (fst protoDia,mp)

dia5_2 = do
    cmmi5 <- loadFont "cmmi5.svg"
    let opt = def{textFont = cmmi5}
        txt2 = textSVG_ opt "ABCDE" # scale 0.15 # fc black # lw none
    return txt2