{-# LANGUAGE OverloadedStrings #-}
module FreeNote.FN202011 where

import Parts hiding(B)
import DiagramLanguage
import Diagrams.Prelude
import qualified Algebra.Graph as Alga hiding ((===))
import Data.Maybe (fromMaybe,isNothing)
import Diagrams.TwoD.Arrow
import Diagrams.BoundingBox
import qualified Control.Lens as Lens (at,(?~))
import qualified Data.Map as Map
import Data.Char
import CmSymbols
import Diagrams.Backend.PGF hiding(B)
import PGFSurface
import Data.String

type B = SVG


-- 引き戻しの図式を回転させてみた
    -- あとgenDiagramの試運転
dia1_1 =
    let rot = rotateBy (-1/8)
        loctrl = rot $ fromOffsets [unitX,unitY,unit_X,0.5*^(unit_X + unitY)]
        labels = map (flip boxedText 0.15) ["A","C","B","P","X"]
        alga   = 5*(1+4+3) + 4*(1+3) + (1+3)*2
        plbSymbol = plb # translateY 0.75 # rot
    in genDiagram loctrl labels id alga <> plbSymbol


-- MorphOptsをLensで編集する練習
dia2_1 =
    let loctrl = fromOffsets [unitX + 0.5 *^ unitY] :: Trail V2 Double
        lab = attachLabel (loctrl `at` origin) (boxedText "f" 0.15 :: Diagram PGF) 0.5
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

_monic (Morph loctrl opts symbs acts) = 
    let p1 = atParam loctrl 0 
    in over locTrail (flip at p1 . monicShaft . unLoc) (Morph loctrl opts symbs acts)

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
    in genDiagram loctrl objs id alga

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
    in genDiagram loctrl objs id alga

-- 図式言語のお試し
    -- genGraphLocTrailの仕様に難あり。[LocatedTrail]と[Diagram B]でatPointsするのではAlgaで制御できなくなる
dia3_3 = do
    let f alga =
            let loctrl = fromOffsets [unitY,unit_Y + unitX]
                objs = map (lw none . flip box 0.02 . fc black . strokeP . flip textSVG 0.2) ["M","A","B"]
                --alga = 2*(1+3) + 1*3
                (disd,mp) = genGraphLocTrail loctrl objs alga
                mp' = over (Lens.at (uncurry Single (1,3))) (fmap monic) mp
            in mkDiagram (disd,mp')
    v1 <- vline 1.7 Forall
    v2 <- vline 1.7 Only
    return $ f (1*3) ||| v1 ||| f ((1+2)*3) ||| v2 ||| f ((1+2)*3 + 2*1)

dia3_3' = do
    let f alga =
            let loctrl = fromOffsets [unitY,unit_Y + unitX]
                objs = replicate 3 bc
                (disd,mp) = genGraphLocTrail loctrl objs alga
                mp' = fmap (\opts -> set (arrOpts . headLength) (local 0.06) . set (arrOpts . gaps) (local 0.02) $ opts)   $ over (Lens.at (Single 1 3)) (fmap monic) mp
            in mkDiagram (disd,mp')
    v1 <- vline 1.7 Forall
    v2 <- vline 1.7 Only
    return $ f (1*3) ||| v1 ||| f ((1+2)*3) ||| v2 ||| f ((1+2)*3 + 2*1)

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
    in genDiagram loctrl objs id alga

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

dia4_2 = do
    let trl = fromOffsets [unitY,unit_Y + unitX]
        objs = replicate 3 bc
        alga1 = 1*3
        alga2 = (1+2)*3
        alga3 = (1+2)*3 + 2*1
        --(disd,opmap) = genGraphLocTrail trl objs alga2
        diagrams = map (genGraphLocTrail trl objs) [alga1,alga2,alga3]
        pmap' xmap = xmap & Lens.at (Single 1 3) %~ fmap equalizer
        diagrams' = map (over _2 pmap') diagrams
    v <- vline 1.7 Forall
    return $ foldr (\x y -> x ||| v ||| y) mempty $ map mkDiagram diagrams'

-- 図式言語の量化記号訂正版
dia4_2' = do
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
    vlines <- verticals height [NoLine,Forall,ExistsOnly]
    return $ foldr (|||) mempty $ zipWith (|||) vlines diagrams'

-- 図式言語手習い
    -- 「任意のcoverはepiである」という主張
dia4_3 = do
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
    vlines <- verticals height [Forall,Forall,Only]
    return $ foldr (|||) mempty $ zipWith (|||) vlines diagrams'

-- 図式言語手習い「coverの定義」
    -- 未完成。Twinの自動化と、平行で逆向きな二つの射を描くための関数を用意しなければならない
dia4_4 = do
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
    vlines <- verticals height [NoLine,Forall,Exists]
    return $ foldr (|||) mempty $ zipWith (|||) vlines protoDia'

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

-- Computer ModernのフォントをSVGFontsで使いこなす方法の模索に入る
dia5_2 = do
    cmmi5 <- loadFont "cmmi5.svg"
    let opt = def{textFont = cmmi5}
        txt2 = textSVG_ opt "ABCDE" # scale 0.15 # fc black # lw none
    return txt2

-- Unicodeの16進数をInt値に変換
    -- HaskellのStringはUnicodeを\(10進数)の形式で表す
    -- そしてこの形式でないとtextSVG関数たちがフォントファイルを参照してくれない模様
-- encode =
--     let trans n [] = n
--         trans n (x:xs) = trans (digitToInt x + 16 * n) xs
--     in trans 0

-- -- いい感じで作ったけど今回は必要ないものだった
-- -- decode =
-- --     let change xs 0 = map intToDigit xs
-- --         change xs n =
-- --             let m  = mod n 10
-- --                 n' = div n 10
-- --             in change (m:xs) n'
-- --     in change []

-- utfInHask = chr . encode
-- まとめてCmSymbolsに移転

dia6_1 = do
    cmmi5 <- loadFont "cmmi5.svg"
    let opt = def{textFont = cmmi5}
        txt2 = textSVG_ opt ("\61504y = fx\61488 f" 
                          ++ map utfInHask["f0ae","f0af","f07e","f0b8","f0bd","f0c0","f0c3","f0a6"]) 
                          # scale 0.15 # fc black # lw none
    return txt2

dia6_2 = do
    cmsy5 <- loadFont "cmsy5.svg"
    let opt = def{textFont = cmsy5}
        txt = lw none $ fc black $ scale 0.15 $ textSVG_ opt 
            $ "A" ++ [utfInHask "f05b"] ++ "B"
            ++ map utfInHask ["f05b","f073","f038","f0a6","f03b","f0a3","f071"] ++ "B"
    return txt

-- unicodeを入力すると、cmsy5から対応する文字を取得して返す関数
    -- 主に数式記号を扱う
    -- IOモナドに包まれてるが、果たして使いやすく丸め込めるか？
-- mathSymbol unics = do
--     cmsy5 <- loadFont "cmsy5.svg"
--     let opts = def{textFont = cmsy5}
--         txt  = map utfInHask unics
--     return $ textSVG_ opts txt
-- CmSymbols.hsに移転

dia6_3 = do
    a <- mathAlphabet "A"
    cup <- cup_
    b <- mathAlphabet "B"
    return $ (a|||cup # scale 1.8|||b) # fc black

-- dia1_1の文字記号をComputerModernにするテスト
dia6_4 = do
    xs <- mapM mathAlphabet ["A","C","B","P","X"]
    let rot = rotateBy (-1/8)
        loctrl = rot $ fromOffsets [unitX,unitY,unit_X,0.5*^(unit_X + unitY)]
        labels = map (scale 0.15 . lw none . flip Parts.box 0.01 . fc black ) xs
        alga   = 5*(1+4+3) + 4*(1+3) + (1+3)*2
        plbSymbol = plb # translateY 0.75 # rot
    return $ genDiagram loctrl labels id alga <> plbSymbol

dia6_5 = do
    l_1 <- fmap (scale (0.12 * (10/9)) . lw none . fc black) $ mathNumber "1"
    l_s <- fmap (scale 0.12 . lw none .  fc black) $ mathAlphabet "s"
    l_r <- fmap (scale 0.12 . lw none . fc black) $ mathAlphabet "r"
    objs <- map (scale 0.14 . lw none . flip Parts.box 0.01 . fc black) <$> mapM mathAlphabet ["P","P","Q"]
    let trl = fromOffsets [unit_X + unitY , unitX]
        alga = 2*(1+3) + 3*1
        protoDia = genGraphLocTrail trl objs alga
        setting mp = mp & Lens.at (Single 2 1) %~ fmap (takeLabel_ l_1 0.5 0.14 False) -- ここらへん抽象化できそう
                        & Lens.at (Single 2 3) %~ fmap (takeLabel l_s 0.5 True)  -- Lens.at キー %~ fmap f のパターン
                        & Lens.at (Single 3 1) %~ fmap (takeLabel l_r 0.5 True)  -- 記述がさらに短くなる予感
    return $ mkDiagram . over _2 setting $ protoDia

dia7_1 = do
    l1 <- mathObject '1'
    objs <- mapM mathObject "PPQ"
    let trl = fromOffsets [0.7 *^ unit_X + unitY , 1.4 *^ unitX]
        alga = (2+3)*1 + 2*3
        cov = over arrOpts cover 
--        mon = over arrOpts monic
        setting mp = mp & Lens.at (Single 2 1) %~ fmap (takeLabel_ l1 0.5 0.14 False)
                        & Lens.at (Single 3 1) %~ fmap cov
                        & Lens.at (Single 2 3) %~ fmap monic
    return $ mkDiagram . over _2 setting $ genGraphLocTrail trl objs alga 

dia7_2 = 
    let rec1 = rect 1 2
        rec2 = rect (sqrt 2) 2
        tri = fromOffsets [unitY,unitX + unit_Y,unit_X] # centerXY
        ps1 = map p2 [(i,j) | j <- [-1,1] , i <- [-0.5,0.5,1.5,1.5+sqrt 2]]
        ps2 = map p2 [(0.5,i) | i <- [-2,2]]
        --pois = zipWith named ([1..] :: [Int]) $ map (place bc) (ps1 ++ ps2) :: [Diagram B]
    in (rec1 ||| vcat [tri,rec1,tri # scaleY (-1)] # centerXY ||| rec2) 

dia7_3 = do
    --labels <- map (scale 0.12 . lw none . flip Parts.box 0.01 . fc black) <$> mapM mathAlphabet ["E","D","E","F","E","B","C","B","A","B"]
    let trl = fromOffsets [unitX,unit_Y,unitX+unitY,unitX,2*^unitY,unit_X,unit_X+unitY,unit_Y,unit_X]
        objs = replicate 10 (square 0.0001 # fc black)
        alga = 2*(1+10+9+7+3+4) + 9*(10+8+7) + 7*(8+4+6) + 4*(3+5+6) + 1*10 + 5*6
        (g,mp) = genGraphLocTrail trl objs alga
        mp' = fmap (set (arrOpts . arrowHead) noHead . set (arrOpts.gaps) (local 0)) mp
--         labs = mconcat $ zipWith ($) (zipWith attachLabel trl (labels :: [Diagram B])) $ (map (*0.1) [0,1,2,3,4,5,6,7,8,9] :: [Double])
    return $ mkDiagram (g,mp')

dia8_1 = do
    x <- mathObject 'x'
    y <- mathObject 'y'
    a <- mathObject 'A'
    i <- mathObject 'i'
    lbr <- fmap (scale 0.12 . lw none . flip Parts.box 0.01 . fc black) (mathNumber "(")
    rbr <- fmap (scale 0.12 . lw none. flip Parts.box 0.01 . fc black)  (mathNumber ")")
    colim <-  scale 0.15. lw none . flip Parts.box 0.01 . fc black <$> mathNumber "colim"
    let trl = fromOffsets [unitY,unitX + 0.5 *^ unitY, unit_Y]
        objs = [x,y,hcat [colim,a] ,hcat[a,lbr,i,rbr]]
        alga1 = 1*2 + 2*3
        alga2 = 1*(2+4) + 2*3 + 4*3
    return $ diagramLanguage [Forall,Exists]  (map (mkDiagram . genGraphLocTrail trl objs) $ [alga1,alga2])

-- 有向集合の記号
dia10_1 = 
    let trl = fromOffsets [unitX + 2 *^ unitY,unitX + 2 *^ unit_Y] :: Trail V2 Double
    in stroke trl :: Diagram B

-- 有向集合（上限つき）
dia10_2 =
    let trl = fromOffsets [unitX + 2 *^ unitY,unitX + 2 *^ unit_Y] :: Trail V2 Double
        top = place bc (1 ^& 2)
    in stroke trl <> top

-- 下向きの有向集合
dia10_1' = dia10_1 # scaleY (-1)
-- 下向きの有向集合(下限付き)
dia10_2' = dia10_2 # scaleY (-1)


dia11_1 = do
    objs_ <- mapM mathAlphabet ["E","X","E","P","D"] :: IO [Diagram PGF]
    labs_ <- mapM mathAlphabet $ map return "hlrfg" :: IO [Diagram PGF]
    let arrange = map (scale 0.15 . lw none . flip Parts.box 0.01 . fc black) 
        objs = arrange objs_
        labs = arrange labs_
        trl = fromOffsets[unitX,unitY,unit_X,0.5*^(unit_X + unitY)]
        alga1 = 5*(1+3) + (1+3)*2
        alga2 = alga1 +4*(1+3)
        qs = [Forall,Exists]
        (disd,trlmp) = genGraphLocTrail trl objs alga2
    return disd

renderpgf name = renderOnlinePGF name (mkSizeSpec2D (Just 600) (Just 450))

-- やっと気づいたが、PGFバックエンドはSVGバックエンドと物物のサイズの尺度が違う
    -- scale 0.15でも全然デカイ。どんな縮尺になっているのか詳しく調べた方が良い
    -- それさえ突き止めれば、今まで作った関数はPGF向けにも使える。
dia13_1 = do
    objs <- mapM getPGFObj ["E","X\\times Y","\\int_0^1 E(x)\\mathrm{dx}","P","D\\bigotimes E"] :: OnlineTex [Diagram PGF]
    labs <- mapM getPGFLabel ["f_1^q","f","g","h"] :: OnlineTex [Diagram PGF]
    test <- hboxOnline "A" :: OnlineTex (Diagram PGF)
    let trl = map (10 *^ ) $ fromOffsets [unitX,unitY,unit_X,0.5 *^ (unit_X + unitY)]
        alga = 5 * (1+3 + 4) + (1+3) * 2 + 4*(1+3)
        plback = plb # scale 10 # translateY 7.5
        qs = [Forall,Exists]
        update mp = mp & (Lens.at (Single 1 2)) %~ (fmap (monic . over arrOpts cover . takeLabel (view (ix 0) labs # centerXY # lw none) 0.5 False))
        disd = return $ (mkDiagram $ over _2 update $ genGraphLocTrail trl objs alga) <> plback
        --disd = return $ genDiscrete trl objs alga ||| place test (1 ^& 4)
    disd

-- hboxOnline系。いい感じなのでDiagramLanguageに移籍
-- between xs ys zs = xs <> zs <> ys

-- mathEnv = between "$" "$"

-- getPGFSymbol d xs = do
--     lab <- hboxOnline . mathEnv $ xs
--     return $ lab # scale d # centerXY

-- getPGFLabel = getPGFSymbol 0.01

-- getPGFObj = getPGFSymbol 0.015

dia13_2 = do
    objs <- mapM getPGFObj ["E","X\\times Y", "\\int_0^1 E(x) \\mathrm{dx}", "P" , "D\\bigotimes E \\bigotimes \\displaystyle{\\int_0^{10} f(x) \\mathrm{dx}}"] :: OnlineTex [Diagram PGF]
    labs <- mapM getPGFLabel ["f_1","f_2","g","h"] :: OnlineTex [Diagram PGF]
    let sc = 10
        trl = map (sc *^) $ fromOffsets [unitX,unitY,unit_X , 0.5 *^ (unit_X + unitY)]
        alga1 = 4*(1+3) + (1+3)*2
        alga2 = alga1 + 5*(1+3)
        alga3 = alga2 + 5*4
        qs = [Exists,Forall,Exists] -- 量化記号もhboxOnlineで取得するべきか？
        plbac = plb # scale sc
        update mp = mp & (Lens.at (Single 4 1)) %~ fmap (takeLabel_ plbac 0.23 0 True)
        ds = map (alignB . mkDiagram . over _2 update . genGraphLocTrail trl objs) [alga1,alga2,alga3]
    return $ diagramLanguage qs ds

-- DCPO上の拡大列(expanding seqence)関手の図
dia13_3 = do
    objs_ <- mapM getPGFObj ["D(1)","D(2)","D(3)","\\cdots"]
    labs  <- mapM getPGFLabel ["1","e_{21}","e_{32}","e_{43}","p_{12}","p_{23}","p_{34}"]
    let sc   = 10
        trl  = fromOffsets [unit_X + unitY , unitX , unitX,unit_Y, unitX,unitY]
        d1   = objs_ ^. ix 0
        d2   = view (ix 1) objs_
        d3   = view (ix 2) objs_
        idDi = labs ^. ix 0
        e32  = labs ^. ix 2
        e43  = labs ^. ix 3
        p12  = labs ^. ix 4
        p23  = labs ^. ix 5
        p34  = labs ^. ix 6
        dots = view (ix 3) objs_
        objs = [d1,d1,d2,d3,d2,d3,dots]
        alga = (2+3)*1 + 2*3 + 3*(4+5) + 4*5 + 4*(6+7) + 7*6
        g i j h   = over (Lens.at (Single i j)) (fmap h) -- これは中々良い抽象化
        f i j x b = g i j (takeLabel x 0.5 b)          -- こうやって部分適用できて応用が広い
        update  = over (Lens.at (Single 2 3)) (fmap (monic . takeLabel (view (ix 1) labs) 0.5 True))
                . over (Lens.at (Single 3 4)) (fmap (monic . takeLabel e32 0.5 True))
                . over (Lens.at (Single 2 1)) (fmap $ takeLabel idDi 0.5 False)
                . over (Lens.at (Single 3 1)) (fmap $ takeLabel p12 0.5 True)
                . tackLabel 3 5 idDi False--over (Lens.at (Single 3 5)) (fmap $ takeLabel idDi 0.5 False)
                . f 4 5 p34 True
                . f 4 6 idDi False
                . actOpt 4 7 monic--over (Lens.at (Single 4 7)) (fmap monic)
                . f 4 7 e43 True
                . f 7 6 p34 True
    return $ mkDiagram . over _2 update $ genGraphLocTrail trl objs alga

-- MorphOptsのMapに対する作用素
    -- (i,j)をキーに持つArrowOptsにアクセスして、MorphOpts -> MorphOpts関数を適用
    -- 必然的に、射に装飾を施す関数はMorphOpts -> MorphOptsの形にすることが規格となる
    -- 無印はSingleと対応。_Twin付きはTwinキーと対応。
actOpt i j f = over (Lens.at (Single i j)) (fmap f)
actOpt_Twin i j b f = over (Lens.at (Twin i j b)) (fmap f)
-- おそらく最も頻出であろう、ラベルを貼るための関数
    -- 標準的なものはLocatedTrailの中間点にラベルを貼る
tackLabel i j l b = actOpt i j (takeLabel l 0.5 b)
tackLabel_ i j l b d1 d2 = actOpt i j (takeLabel_ l d1 d2 b)

-- Exponential Categoryの定義
dia13_4 = 
    let objs = replicate 6 bc
        trl = fromOffsets [unit_Y + unitX, unit_X, 1.5 *^ unit_X, 2.5 *^ unitX + unitY , 1.5 *^ unitY]
        alga1 = 1 + 2
        alga2 = 3*(1+2) + 3*4
        alga3 = alga2 + 5*(1+2+6)
        alga4 = 5*(1+2+3+6) + 3*(1+2+4) + 6*4
        qs = [Forall,Exists,Forall,ExistsOnly]
        update mp = mp & fmap (arrOpts.headLength .~ (local 0.08)) -- Lensの中にこうやってfmapを混ぜ込める
                       & actOpt 3 1 (takeLabel_ (prd # scaleX (-1)) 0.25 0 False)
                       & actOpt 5 6 (takeLabel_ (prd # scaleX (-1))  0.16 0 True)
        ds = map (alignB . rotateBy (1/8)) . over (ix 0) (atop (place (circle 0.001 # lw none) (unit_Y + 1.5 *^ unit_X)))
           $ map (genDiagram trl objs update) [alga1,alga2,alga3,alga4] -- これは抽象化できるのではないか？
    in diagramLanguage qs ds

-- mkDiagram . over _2 update . genGraphLocTrail trl objs
-- この形式で図式を十分に装飾しつつ生成できるはず
-- 名前はgenDiagramがふさわしいので、既存の関数をアップグレードする感じで実装

dia14_1 = do
    objs_ <- mapM getPGFObj ["X \\times A","X \\times B^X","B"]
    labs_ <- mapM getPGFLabel ["1_X \\times \\lambda \\varphi", "ev_B","\\varphi"]
    let trl = fromOffsets [unitY,unit_Y + unitX]
        o i = view (ix i) objs_
        l i = view (ix i) labs_
        objs = [o 1,o 0,o 2]
        alga = 2*(1+3) + 1*3
        update = tackLabel 2 3 (l 2) True
               . tackLabel 1 3 (l 1) False
               . tackLabel_ 2 1 (l 0) False 0.5 0.2
               . fmap (over (arrOpts.headLength) (0.8*))
    return $ genDiagram trl objs update alga

dia14_2 = do
    objs_ <- mapM getPGFObj ["A"
                           ,"B"
                           ,"A \\times B^A" 
                           ,"B^A" 
                           ,"A \\times X"
                           ,"X"]
    labs_ <- mapM getPGFLabel ["f"
                             ,"\\lambda f"
                             ,"1 \\times \\lambda f"
                             ,"ev"]
    let trl = rotateBy (1/8) $ fromOffsets [unitX + unit_Y,unit_X,1.5 *^ unit_X, 2.5 *^ unitX + unitY, 1.5 *^ unitY]
        o i = view (ix (i-1)) objs_
        l i = view (ix (i-1)) labs_
        alga1 = 1 + 2
        alga2 = alga1 + 3*(1+2+4)
        alga3 = alga2 + 5*(1 + 2 + 6)
        alga4 = alga3 + 5*3 + 6*4
        update = tackLabel 3 2 (l 4) False
               . tackLabel 5 2 (l 1) True
               . tackLabel_ 5 3 (l 3) True 0.5 0.16
               . tackLabel 6 4 (l 2) False
               . tackLabel_ 3 1 (prd # scaleX (-1) # rotateBy (1/8)) True 0.19 0
               . tackLabel_ 5 1 (prd # rotateBy (1/8 + 1/4) # alignB) True 0.20 0
        qs = [Forall,Exists,Forall,ExistsOnly]
        ghost = (place (circle 0.001 # lw none) (unit_Y + 1.5 *^ unit_X)) # rotateBy (1/8)
        putGhost = over (ix 0) (alignB . atop ghost)
        ds = putGhost $ map (alignB . genDiagram trl objs_ update) [alga1,alga2,alga3,alga4]
    diagramLanguage qs ds

-- exponential関手(-)^Xを随伴から構成する図式言語
dia14_3 = do
    objs <- mapM getPGFObj [
                            "A"
                           ,"X \\times A^X"
                           ,"B"
                           ,"X \\times B^X"
                           ,"B^X"
                           ,"A^X"
                           ]
    labs <- mapM getPGFLabel ["f"
                             ,"\\overline{f}"
                             ,"ev"
                             ,"(ev)f"
                             ,"\\lambda (ev) f"
                             ,"1 \\times \\lambda ((ev) f)"
                             ]
    let trl = fromOffsets [unitY,unitX,unit_X+unitY,unit_X,unit_Y]
        alga1 = 1*3
        alga2 = alga1 + 2*(1+3+6) + 4*(5+3)
        alga3 = alga2 + 2*4 + (6+4)*5
        l i   = view (ix $i-1) labs
        update = tackLabel 1 3 (l 1) False
               . tackLabel 2 1 (l 3) False
               . tackLabel 2 3 (l 4) True
               . tackLabel 4 3 (l 3) True
               . tackLabel_ 2 4 (l 6) True 0.5 0.3
               . tackLabel_ 6 5 (l 5) True 0.5 0.2
        ds = map (alignB . genDiagram trl objs update) [alga1,alga2,alga3]
        qs = [Forall,Exists,ExistsOnly]
    diagramLanguage qs ds

-- epic射の矢印記号
    -- dartを二つ連ねた鏃
    -- これをPathのままArrowOptsのArrowHeadにsetすればepicの矢印が手に入るはずだ
dia14_4 = 
    let p1 = fst $ dart 1 0
        trl = mconcat . map unLoc $ pathTrails p1 -- dartのArrowHeadをバラしてTrailに
        p2 = Path [trl `at` origin,trl `at` 1.5 *^ unitX] -- Located TrailのリストからPathを構成
        epicHead x y = (p2 # scale x,p2 # scale y)
        arr = arrowV' (with & arrowHead .~ epicHead) (10 *^ unitX)
    in return $ arr <> circle 100

-- epic射を表す矢印記号用のArrowHead値
    -- 二つの引数はdartの中で正しく使われているので、dartが正しく実装されていればこの関数も常に正しく動く
    -- DiagramLanguageに移動してepic :: MorphOpts -> MorphOpts値に仕立て上げるべし。
-- epicHead x y = 
--     let p = fst $ dart x y
--         trl = mconcat . map unLoc $ pathTrails p
--         l   = diameter unitX p
--         p2 = Path [trl `at` origin,trl `at` (0.8*l) *^unitX]
--     in (p2,snd $ dart x y)

dia14_5 =
    let trl = origin ~~ unitX
        arr = evalMorphOpts $ def & locTrail .~ trl & epic
    in return $ arr <> square 1

dia14_6 =
    let trl = fromOffsets [unitX,unitY]
        objs = replicate 3 bc
        alga1 = 1*2
        alga2 = alga1 + 1*3
        alga3 = alga2 + 2*3
        update = actOpt 1 2 epic 
               . actOpt 1 2 ((set (arrOpts.headGap) (local (-0.03))) . (over actions ((lc red):)))
        ds = map (genDiagram trl objs update) [alga1,alga2,alga3]
        qs = [NoLine,Forall,Only]
    in diagramLanguage qs ds

dia15_1 = do
    objs_ <- mapM getPGFObj ["A","B","C","D","1","2","3","4","5","6"]
    let o i = view (ix i) objs_
        trl1 = fromOffsets [unitX , unit_Y, unitX + 0.5 *^ unitY] ++ map (2 *^ unit_Y +) (fromOffsets [unitX + 0.5 *^ unitY,unitX, 0.5 *^ unitX + 1.5 *^ unit_Y,unit_Y + unit_X,unit_X])
        alga1 =  1*(2+3) + (2+3)*4 -- 二つのグラフの間に矢印を引きたいのだが、名前が重複してしまってる
        alga2 = 5*(6+7+9+10) + 6*(8+10) + 7*(8+9) -- 同じ名前で
        update = id
        c = connectOutside
        -- obj1 = take 4 objs_
        -- obj2 = drop 4 objs_
    return $ genDiagram trl1 objs_ id (alga1 + alga2) -- === strutY 1 === genDiagram trl2 obj2 id alga2)
           # connectOutside (1 :: Int) (5 :: Int) 

dia17_1 = do
    objs_ <- mapM (getPGFObj . \x -> between "\\text{" "}" x) ["テスト！","テスト？","テスト。"]
    let trl1 = fromOffsets [unitX + unitY,unitX + unitY]
        alga = Alga.path [1,2,3]
        update = id
        d = genDiagram trl1 objs_ update alga
    diagramLanguage [NoLine] [d]

-- 図式の対象に日本語を使えるようにする
getJpnObj = getPGFObj . between "\\text{" "}"
-- 射のラベルにも日本語を使えるように。
getJpnLabel = getPGFLabel . between "\\text{" "}"

dia17_2 = do
    jpnobj <- mapM getJpnObj ["\\texttt{Rtree a -> [[a]]}で実現できそう"
                             ,"各リストの中身を\\texttt{place}"
                             ,"その際、x座標だけ使い、y座標は0にする"
                             ,"リスト毎に\\texttt{mconcat}"
                             ,"\\texttt{diameter unitX}で幅を測り、\\texttt{vline}"
                             ,"\\texttt{root===vline===subtree}の形の再帰関数"
                             ,"y座標を0にしたのは、\\texttt{===}を活用するため。"]
    let trl = fromOffsets [0.5 *^ unit_Y, unitX + 0.5 *^ unit_Y,0.5 *^ unit_Y]
        o i = view (ix (i-1)) jpnobj
        objs = [o 1, o 2 === o 3,o 4,o 5 === o 6 === o 7]
        alga = Alga.path [1,2,3,4]
    return $ genDiagram trl objs id alga

instance IsString a => IsString (Alga.Graph a) where
    fromString      = Alga.Vertex . fromString

(+++) :: IsString a => Alga.Graph a -> Alga.Graph a -> Alga.Graph a
xs +++ ys = Alga.Overlay xs ys

(***) :: IsString a => Alga.Graph a -> Alga.Graph a -> Alga.Graph a
xs *** ys = Alga.Connect xs ys

dia17_3 = do
    objs <- mapM getPGFObj ["H","C","D","G","A","B","E","F"]
    let 
        alga = 1*(2+3+4) + 2*(5+6) + 4 * (7+8)
        tree_ = genTree 1 alga
        f i = view (ix (i-1)) objs
        tree = fmap (scaleY (-1) . f) tree_
        verticeGraph = symmLayout tree
        dia  = scaleY (-1) $ foldr (<>) mempty $ fmap (\(x,p) -> place x p) verticeGraph
    return dia