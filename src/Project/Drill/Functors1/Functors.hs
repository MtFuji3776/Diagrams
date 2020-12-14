module Project.Drill.Functors1.Functors where

import Parts
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))
import DiagramLanguage
import Diagrams.Backend.PGF
import PGFSurface hiding (easyRender,ds,easyRender')
import ProofTree

-- fig:~~毎に分類して保管していく

easyRender name = PGFSurface.render ("/Users/fujimotomakoto/haskell_testing/diagrams/src/Project/Drill/Functors1/img/" <> name <> ".tex")

easyRender' w h name = PGFSurface.render' w h ("/Users/fujimotomakoto/haskell_testing/diagrams/src/Project/Drill/Functors1/img/" <> name <> ".tex")

outputName = ["fig_cat2"
        ,"fig_def1"
        ,"diaId"
        ,"diaProd"
        ,"q3"
        ,"derivHomCov"
        ,"homCovUnit1"
        ,"homCovUnit2"
        ,"homCovMor"
        ,"derivProd"]
ds = [fig_cat2
         ,fig_def1
         ,diaId
         ,diaProd
         ,q3
         ,derivHomCov
         ,homCovUnit1
         ,homCovUnit2
         ,derivProd]

-- fig:cat2
fig_cat2 = do
    objs <- mapM getPGFObj ["A_1","A_2","A_3","\\mathbf{A}:"]
    labs <- mapM getPGFLabel ["f_1","f_2"]
    objs2 <- mapM getPGFObj ["\\textcolor{red}{FA_1}","\\textcolor{red}{FA_2}","B_3","\\textcolor{red}{FA_3}","\\mathbf{B}:"]
    labs2 <- mapM getPGFLabel ["\\textcolor{red}{Ff_1}", "g_2","g_3","\\textcolor{red}{Ff_2}","g_5","g_1g_3","\\downarrow F"]    
    let alga = path [1,2,3]
        trl = fromOffsets [unitX,unitX] 
        o i = view (ix (i-1)) objs
        l i = view (ix (i-1)) labs
        update = tackLabel 1 2 (l 1) True . tackLabel 2 3 (l 2) True
        d = genDiagram trl objs update alga <> place (view (ix 3) objs) (0.3 *^unit_X)
        -- ここから関手の像を含んだ圏B
        trl2 = fromOffsets [unitX,unit_Y,unitX]
        alga2 = (2+4)*3 + 1*2 + 2*4
        l2 i = view (ix (i-1)) labs2
        update2 = actOpt_Twin 1 2 False (set actions [lc red])
                . actOpt 2 4 (set actions [lc red]) . tackLabel 4 3 (l2 5) True
                . tackLabelTwin 1 2 True (l2 2) True . tackLabelTwin 1 2 True reticle False
                . tackLabelTwin 1 2 False (l2 1) False
                . (tackLabel 2 4 (l2 4) True ) . tackLabel 4 5 (l2 5) True
                . tackLabel 2 3 (l2 3) True . introTwin 1 2
        d2 = genDiagram trl2 objs2 update2 alga2 <> place (view (ix 4) objs2) (0.3 *^ unit_X)
    return $ d === strutY 0.2 === place (view (ix 6) labs2) (0.3 *^ unit_X) === strutY 0.1 === d2

-- 共変関手の公理1,2
fig_def1 = do
    objs <- mapM getPGFObj ["A_1"  -- 1
                           ,"A_2"
                           ,"FA_1" -- 3
                           ,"FA_2"
                           ,"F"]   -- 5
    labs <- mapM getPGFLabel ["\\Box f"                 -- 1
                             ,"f"
                             ,"f\\Box"                 -- 3
                             ,"F(\\Box f)"
                             ,"Ff"                      -- 5
                             ,"F(f \\Box)"]
    let alga = 1*(1+2)*2
        trl = fromOffsets [unitX]
        o i = view (ix (i-1)) objs
        l i = view (ix (i-1)) labs
        update1 = tackLabel 1 1 (l 1) True
                . tackLabel 1 2 (l 2) True
                . tackLabel 2 2 (l 3) True
        update2 = tackLabel 1 1 (l 4) True
                . tackLabel 1 2 (l 5) True
                . tackLabel 2 2 (l 6) True
        vl = strutY 0.1 === (translateX (-0.1) $ hrule 1.3 # alignL # lw thick) ||| o 5 # translateY (-0.05) === strutY 0.1
        d1 = genDiagram trl (map o [1,2]) update1 alga
        d2 = genDiagram trl (map o [3,4]) update2 alga
    return $ pad 1.5 $ d1 === vl === d2

-- 多用する圏A,Bの例
catAB = do
    objs1 <- mapM getPGFObj ["A_1","A_2","A_3"]
    labs1 <- mapM getPGFLabel ["f_1","f_2"]
    a <- getPGFObj "\\mathbf{A}:"
    let trl1 = fromOffsets [unitX,unitX]
        alga1 = path [1,2,3]
        l1 i = view (ix $ i - 1) labs1
        update1 = tackLabel 1 2 (l1 1) True . tackLabel 2 3 (l1 2) True
        d1 = a ||| strutX 0.05 ||| genDiagram trl1 objs1 update1 alga1
    objs2 <- mapM getPGFObj ["B_1","B_2","B_3","B_4"]
    labs2 <- mapM getPGFLabel ["g_1","g_2","g_3","g_4","g_5"]
    b <- getPGFObj "\\mathbf{B}:"
    let trl2 = fromOffsets [unitX,unitX + unit_Y,unit_X]
        alga2 = path [1,2,3,4] + 2*4
        l2 i = view (ix $ i - 1) labs2
        update2 = tackLabelTwin 1 2 True (l2 1) True . tackLabelTwin 1 2 False (l2 2) False . tackLabel 2 3 (l2 4) True . tackLabel 3 4 (l2 5) True . tackLabel 2 4 (l2 3) False
                . introTwin 1 2
        mark = place reticle (0.5 *^ unitX)
        d2 = b ||| strutX 0.05 ||| (genDiagram trl2 objs2 update2 alga2 <> mark)
    return $ d1 === strutY 0.5 === d2

-- ============================= 1.2 色々な関手 =========================
-- 恒等関手Id
diaId = do
    objs <- mapM getPGFObj ["A","B"]
    lab <- getPGFLabel "f"
    labID <- getPGFLabel "1_{\\mathbf{A}}"
    let trl = fromOffsets [unitX]
        alga = 1*2
        update = tackLabel 1 2 lab True 
        d = genDiagram trl objs update alga # centerXY
        hor = centerXY $ hrule 1.5 ||| labID 
    return $ vsep 0.1 [d,hor,d]



-- - × Aの形式の１変数関手
diaProd = do
    objs_ <- mapM getPGFObj ["X","X \\times A","A"]
    labs  <- mapM getPGFLabel["1_X" --1
             ,"\\pi_1"
             ,"\\pi_2" --3
             ,"1_X\\times 1_A"
             ,"1_A" ]--5
    let trl = fromOffsets [unit_X + 0.5 *^ unit_Y , unitY, unitX + 0.5 *^ unitY,unitX + 0.5 *^ unit_Y,unit_Y]
        alga = 4*(1+3+5) + (1 + 3)*2 + (5+1)*6
        o i = view (ix (i-1)) objs_
        l i = view (ix (i-1)) labs
        objs = map o [2,1,1,2,3,3]
        update = tackLabel 1 2 (l 2) True
               . tackLabel 1 6 (l 3) False
               . tackLabel 3 2 (l 1) False
               . tackLabel 4 3 (l 2) False
               . tackLabel_ 4 1 (l 4) True 0.5 0.2
               . tackLabel 5 6 (l 5) True
               . tackLabel 4 5 (l 3) True
        d = genDiagram trl objs update alga # pad 1.2
    return d

derivHomCov = do
    objs <- mapM getPGFObj ["A \\stackrel{\\varphi}{\\to} X \\stackrel{f}{\\to} Y","A \\stackrel{\\varphi}{\\to} X","(A,f)"]
    let alga = 1*2
        f i = view (ix $ i - 1) objs
        t = derivTree f 1 alga # centerXY
        d = t ||| f 3 # scale 0.8
    return d

homCovUnit1 = do
    objs <- mapM getPGFObj ["A \\stackrel{\\varphi}{\\to} X \\stackrel{\\Box f}{\\to} X","A \\stackrel{\\varphi}{\\to} X","(A,\\Box f)"]
    let alga = 1*2
        f i = view (ix $ i - 1) objs
        t = derivTree f 1 alga # centerXY ||| f 3 # scale 0.8
    return t

homCovUnit2 = do
    objs <- mapM getPGFObj ["A \\stackrel{\\psi}{\\to} Y \\stackrel{f \\Box}{\\to} Y","A \\stackrel{\\psi}{\\to} Y","(A,f \\Box)"]
    let alga = 1*2
        f i = view (ix $ i - 1) objs
        t = derivTree f 1 alga # centerXY ||| f 3 # scale 0.8
    return t    

homCovMor = do
    objs <- mapM getPGFObj ["A \\stackrel{\\varphi}{\\to} X \\stackrel{f}{\\to} Y \\stackrel{g}{\\to} Z","A \\stackrel{\\varphi}{\\to} X \\stackrel{f}{\\to} Y","A \\stackrel{\\varphi}{\\to} X","(A,f)","(A,g)","(A,fg)"]
    let alga = 2*3
        f i = view (ix $ i - 1) objs
        subt = derivTree f 2 alga # centerXY ||| f 4 # scale 0.8
        d = 1.01 * diameter unitX subt
        t = centerXY (subt === strutY 0.01 === (hrule d # lw veryThin ||| f 5 # scale 0.8) === strutY 0.01 === f 1)
        -- ここから二つめの導出図
        alga1 = 1*2
        t' = centerXY $ derivTree f 1 alga1 # centerXY ||| f 6 # scale 0.8
    return $ t ||| strutX 0.1 ||| t'

derivProd = do
    objs <- mapM getPGFObj [
        "X \\times A \\stackrel{1_X \\times f}{\\to} {X \\times B}"
        ,"A \\stackrel{f}{\\to} B"
        ,"X \\times f"]
    let alga = 1*2
        f i = view (ix $ i-1) objs
        t = derivTree f 1 alga # centerXY ||| f 3 # scale 0.8
    return t

-- ========================出題の図式

ordsetP = do
    objs <- mapM (getPGFObj.show) [0..10]
    let trl = fromOffsets $ replicate 10 unitX
        alga = path [1..11]
    return $ genDiagram trl objs id alga

catA'B = do
    objs1 <- mapM getPGFObj ["A_1","A_2","A_3","A_4"]
    labs1 <- mapM getPGFLabel ["f_1","f_2"]
    a <- getPGFObj "\\mathbf{A}:"
    let trl1 = fromOffsets [unitX,unitX,unitX]
        alga1 = 1*2+3*4
        l1 i = view (ix $ i - 1) labs1
        update1 = tackLabel 1 2 (l1 1) True . tackLabel 3 4 (l1 2) True
        d1 = a ||| strutX 0.05 ||| genDiagram trl1 objs1 update1 alga1
    objs2 <- mapM getPGFObj ["B_1","B_2","B_3","B_4"]
    labs2 <- mapM getPGFLabel ["g_1","g_2","g_3","g_4","g_5"]
    b <- getPGFObj "\\mathbf{B}:"
    let trl2 = fromOffsets [unitX,unitX + unit_Y,unit_X]
        alga2 = path [1,2,3,4] + 2*4
        l2 i = view (ix $ i - 1) labs2
        update2 = tackLabelTwin 1 2 True (l2 1) True . tackLabelTwin 1 2 False (l2 2) False . tackLabel 2 3 (l2 4) True . tackLabel 3 4 (l2 5) True . tackLabel 2 4 (l2 3) False
                . introTwin 1 2
        mark = place reticle (0.5 *^ unitX)
        d2 = b ||| strutX 0.05 ||| (genDiagram trl2 objs2 update2 alga2 <> mark)
    return $ d1 === strutY 0.5 === d2

numbers = do
    objs <- mapM getPGFObj $ map show [0..3] ++ ["\\cdots","n","\\cdots"] 
    let trl = fromOffsets $ replicate 6 unitX
        alga1 = path [1..7]
        alga2 = path [7,6..1]
        d1 = genDiagram trl objs id alga1 
        d2 = genDiagram trl objs id alga2
    return $ d1 === strutY 0.1 === d2

powerset = do
    let brace x = "\\{" ++ x ++ "\\}"
    objs <- mapM getPGFObj $ ["\\emptyset"] ++ map brace ["1","2","3","3,1","2,3","1,2","1,2,3"]
    let trl = fromOffsets [unit_X + unitY,unitX,unitX,unitY,unit_X,unit_X,unitX + unitY]
        alga = 8*(7+6+5) + (2+3+4)*1 + 7*(2+3) + 6*(3+4) + 5*(2+4)
        d = genDiagram trl objs id alga
    return d


-- ==================================問の証明概要図=====================================================

q3 = do 
    f2 <- getPGFObj "\\forall f \\in \\mathbf{A} (A \\stackrel{f}{\\to} B \\iff B \\stackrel{f}{\\to} A \\text{ in } \\mathbf{A}^{op})"
    f3 <- getPGFObj "\\mathrm{Id}:\\mathbf{A} \\to \\mathbf{A}^{op}"
    f4 <- getPGFObj "\\mathrm{Id}:\\mathbf{A}^{op} \\to \\mathbf{A}"
    return $ vsep 0.2 [f2,f3,f4] # pad 1.2


-- =================== その他

modeltriangle = do
    txts_ <- mapM getPGFText ["作用$\\Leftrightarrow$手続きモデル","構文モデル","公理モデル$\\Leftrightarrow$ 代数"]
    let trl = map (1.5 *^) $ fromOffsets [unitX, 0.5 *^ (unit_X + sqrt 3 *^ unitY)]
        alga = 1*2*3
        txts = over (ix 0) alignR . over (ix 1) alignL $ txts_ 
        f = set (arrOpts . tailLength) (local 0.05) . set (arrOpts.arrowTail) dart'
        update = actOpt 1 2 (set (arrOpts.tailLength) (local 0.05) . set (arrOpts.arrowTail) dart')
                . actOpt 2 3 (f . set (arrOpts.arrowTail) dart)
                . actOpt 1 3 (f . set (arrOpts . arrowTail) dart)
    return $ padded 0.1 $ genDiagram trl txts update alga