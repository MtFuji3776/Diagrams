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
                . tackLabelTwin 1 2 True (l2 2) True  -- . tackLabelTwin 1 2 True reticle False
                . tackLabelTwin 1 2 False (l2 1) False
                . (tackLabel 2 4 (l2 4) True ) . tackLabel 4 5 (l2 5) True
                . tackLabel 2 3 (l2 3) True . introTwin 1 2
        d2 = genDiagram trl2 objs2 update2 alga2 <> place (view (ix 4) objs2) (0.3 *^ unit_X)
        x = place reticle (0.5 ^& 0) # lw thin
    return $ d === strutY 0.2 === place (view (ix 6) labs2) (0.3 *^ unit_X) === strutY 0.1 === (d2  <> x)

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
        vl = strutY 0.1 === (translateX (-0.1) $ hrule 1.3 # alignL # lw thin) ||| strutX 0.05 ||| (o 5 # translateY (-0.05) === strutY 0.1)
        d1 = genDiagram trl (map o [1,2]) update1 alga
        d2 = genDiagram trl (map o [3,4]) update2 alga
    return $ pad 1.5 $ d1 === vl === d2

functorAxiom2Commutativeness = do
    objs1 <- mapM getPGFObj ["A_1","A_2","A_3"]
    labs1 <- mapM getPGFLabel ["f","g","fg"]
    let trl1 = fromOffsets [unitX,unit_Y]
        alga1 = path [1,2,3] + 1*3
        l xs i = view (ix $ i-1) xs
        l1 = l labs1
        update f = tackLabel 1 2 (f 1) True . tackLabel 2 3 (f 2) True . tackLabel 1 3 (f 3) False
        update1 = update l1
        d1 = genDiagram trl1 objs1 update1 alga1
    objs2 <- mapM getPGFObj ["FA_1","FA_2","FA_3"]
    labs2 <- mapM getPGFLabel ["Ff","Fg","F(fg)"]
    let l2 = l labs2
        update2 = update l2
        d2 = genDiagram trl1 objs2 update2 alga1
    func <- getPGFObj "F"
    let line = hrule 1.3 # centerXY # lw thin ||| func
        d = vsep 0.03 [d1 # centerXY , line, d2 # centerXY]
    return d

functorAxiom = do
    x <- fig_def1
    y <- functorAxiom2Commutativeness
    return $ hsep 0.1 $ map centerXY [x,y]


contraFunctorUnitLaw = do
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
        alga' = 2*(1+2)*1
        trl = fromOffsets [unitX]
        o i = view (ix (i-1)) objs
        l i = view (ix (i-1)) labs
        update1 = tackLabel 1 1 (l 1) True
                . tackLabel 1 2 (l 2) True
                . tackLabel 2 2 (l 3) True
        update2 = tackLabel 1 1 (l 4) True
                . tackLabel 2 1 (l 5) True
                . tackLabel 2 2 (l 6) True
        vl = strutY 0.1 === (translateX (-0.1) $ hrule 1.3 # alignL # lw thin) ||| strutX 0.05 ||| (o 5 # translateY (-0.05) === strutY 0.1)
        d1 = genDiagram trl (map o [1,2]) update1 alga
        d2 = genDiagram trl (map o [3,4]) update2 alga'
    return $ pad 1.5 $ d1 === vl === d2

contraFunctorCommutativeLaw = do
    objs1 <- mapM getPGFObj ["A_1","A_2","A_3"]
    labs1 <- mapM getPGFLabel ["f","g","fg"]
    let trl1 = fromOffsets [unitX,unit_Y]
        alga1 = path [1,2,3] + 1*3
        alga1' = path [3,2,1] + 3*1
        l xs i = view (ix $ i-1) xs
        l1 = l labs1
        update f = tackLabel 2 1 (f 1) True . tackLabel 3 2 (f 2) True . tackLabel 3 1 (f 3) False
        update' f = tackLabel 1 2 (f 1) True . tackLabel 2 3 (f 2) True . tackLabel 1 3 (f 3) False
        update1 = update' l1
        d1 = genDiagram trl1 objs1 update1 alga1
    objs2 <- mapM getPGFObj ["FA_1","FA_2","FA_3"]
    labs2 <- mapM getPGFLabel ["Ff","Fg","F(fg)"]
    let l2 = l labs2
        update2 = update l2
        d2 = genDiagram trl1 objs2 update2 alga1'
    func <- getPGFObj "F"
    let line = hrule 1.3 # centerXY # lw thin ||| func
        d = vsep 0.03 [d1 # centerXY , line, d2 # centerXY]
    return d

contraFunctorAxiom = do
    x <- contraFunctorUnitLaw
    y <- contraFunctorCommutativeLaw
    return $ hsep 0.1 $ map centerXY [x,y]


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

-- ====== 反変関手の例

derivContraHom = do
    objs <- mapM getPGFObj ["(X,A) \\stackrel{(f,A)}{\\to} (Y,A)","X \\stackrel{f}{\\to} Y"]
    func <- getPGFObj "(-,A)"
    let alga  = 1*2
        f i = view (ix $ i - 1) objs
        t = derivTree f 1 alga # centerXY
        d1 = t ||| strutY 0.05 ||| func
    objs1 <- mapM getPGFObj ["X \\stackrel{f}{\\to} Y \\stackrel{\\varphi}{\\to} A","Y \\stackrel{\\varphi}{\\to} A"]
    func1 <- getPGFObj "(f,A)"
    colon <- getPGFObj ";"
    let f' i = view (ix $ i - 1) objs1
        t1 = derivTree f' 1 alga # centerXY
        d2 = t1 ||| strutY 0.05 ||| func1
        d = d1 ||| strutX 0.1 ||| colon ||| strutX 0.1 ||| d2
    return d

contraFunctor = do
    objs1 <- mapM getPGFObj ["A_1","A_2","A_3"]
    labs1 <- mapM getPGFLabel ["f_1","f_2"]
    a <- getPGFObj "\\mathbf{A}:"
    let trl1 = fromOffsets [unitX,unitX]
        alga1 = path [1,2,3]
        l1 i = view (ix $ i - 1) labs1
        update1 = tackLabel 1 2 (l1 1) True . tackLabel 2 3 (l1 2) True
        d1 = a ||| strutX 0.05 ||| genDiagram trl1 objs1 update1 alga1
    objs2 <- mapM getPGFObj ["\\textcolor{red}{FA_3}","\\textcolor{red}{FA_2}","B_4","\\textcolor{red}{FA_1}"]
    labs2 <- mapM getPGFLabel ["\\textcolor{red}{Ff_2}","g_1","\\textcolor{red}{Ff_1}","g_4","g_5"]
    b <- getPGFObj "\\mathbf{B}:"
    let trl2 = fromOffsets [unitX,unitX + unit_Y,unit_X]
        alga2 = path [1,2,3,4] + 2*4
        l2 i = view (ix $ i - 1) labs2
        update2 = actOpt_Twin 1 2 True (set actions [lc red])
                . actOpt 2 4 (set actions [lc red]) . tackLabelTwin 1 2 True (l2 1) True . tackLabelTwin 1 2 False (l2 2) False . tackLabel 2 3 (l2 4) True . tackLabel 3 4 (l2 5) True . tackLabel 2 4 (l2 3) False
                . introTwin 1 2
        mark = place reticle (0.5 *^ unitX)
        d2 = b ||| strutX 0.05 ||| (genDiagram trl2 objs2 update2 alga2 <> mark)
    return $ d1 === strutY 0.5 === d2

reverseA = do
    objs1 <- mapM getPGFObj ["A_3","A_2","A_1"]
    labs1 <- mapM getPGFLabel ["f_2","f_1"]
    a <- getPGFObj "\\mathbf{A}^{\\mathrm{op}}:"
    let trl1 = fromOffsets [unitX,unitX]
        alga1 = path [1,2,3]
        l1 i = view (ix $ i - 1) labs1
        update1 = tackLabel 1 2 (l1 1) True . tackLabel 2 3 (l1 2) True
        d1 = a ||| strutX 0.05 ||| genDiagram trl1 objs1 update1 alga1
    objs2 <- mapM getPGFObj ["\\textcolor{red}{FA_3}","\\textcolor{red}{FA_2}","B_4","\\textcolor{red}{FA_1}"]
    labs2 <- mapM getPGFLabel ["\\textcolor{red}{Ff_2}","g_1","\\textcolor{red}{Ff_1}","g_4","g_5"]
    b <- getPGFObj "\\mathbf{B}:"
    let trl2 = fromOffsets [unitX,unitX + unit_Y,unit_X]
        alga2 = path [1,2,3,4] + 2*4
        l2 i = view (ix $ i - 1) labs2
        update2 = actOpt_Twin 1 2 True (set actions [lc red])
                . actOpt 2 4 (set actions [lc red]) . tackLabelTwin 1 2 True (l2 1) True . tackLabelTwin 1 2 False (l2 2) False . tackLabel 2 3 (l2 4) True . tackLabel 3 4 (l2 5) True . tackLabel 2 4 (l2 3) False
                . introTwin 1 2
        mark = place reticle (0.5 *^ unitX)
        d2 = b ||| strutX 0.05 ||| (genDiagram trl2 objs2 update2 alga2 <> mark)
    return $ d1 === strutY 0.5 === d2

-- ============================= 1.2 色々な関手 =========================
-- 恒等関手Id
diaId = do
    objs <- mapM getPGFObj ["A \\stackrel{f}{\\to} B","A \\stackrel{f}{\\to} B"]
    -- lab <- getPGFLabel "f"
    labID <- getPGFLabel "1_{\\mathbf{A}}"
    let trl = fromOffsets [unitX]
        alga = 1*2
        f i = view (ix $ i-1) objs
        t = derivTree f 1 alga # centerXY
        hor = t ||| labID # scale 0.8
    return hor



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
    objs1 <- mapM getPGFObj ["(A,X) \\stackrel{(A,f)}{\\to} (A,Y)","X \\stackrel{f}{\\to} Y"]
    func1 <- getPGFObj "(A,-)"
    let alga  = 1*2
        f' i = view (ix $ i - 1) objs1
        t' = derivTree f' 1 alga # centerXY
        d1 = t' ||| strutY 0.05 ||| func1
    objs <- mapM getPGFObj ["A \\stackrel{\\varphi}{\\to} X \\stackrel{f}{\\to} Y","A \\stackrel{\\varphi}{\\to} X","(A,f)"]
    colon <- getPGFObj ";"
    let f i = view (ix $ i - 1) objs
        t = derivTree f 1 alga # centerXY
        d2 = t ||| f 3 # scale 0.8
        d = hsep 0.1 [d1,colon,d2]
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

homCovUnits = do
    u1 <- homCovUnit1
    u2 <- homCovUnit2
    return $ u1 ||| strutX 1 ||| u2

homCovMor = do
    objs <- mapM getPGFObj ["A \\stackrel{\\varphi}{\\to} X \\stackrel{f}{\\to} Y \\stackrel{g}{\\to} Z","A \\stackrel{\\varphi}{\\to} X \\stackrel{f}{\\to} Y","A \\stackrel{\\varphi}{\\to} X","(A,f)","(A,g)","(A,fg)"]
    objs' <- mapM getPGFObj ["A \\stackrel{\\varphi}{\\to} X \\stackrel{fg}{\\to} Z","A \\stackrel{\\varphi}{\\to} X"]
    let alga = 2*3
        f i = view (ix $ i - 1) objs
        g i = view (ix $ i - 1) objs'
        subt = derivTree f 2 alga # centerXY ||| f 4 # scale 0.8
        d = 1.01 * diameter unitX subt
        t = centerXY (subt === strutY 0.01 === (hrule d # lw veryThin ||| f 5 # scale 0.8) === strutY 0.01 === f 1)
        -- ここから二つめの導出図
        alga1 = 1*2
        t' = centerXY $ derivTree g 1 alga1 # centerXY ||| f 6 # scale 0.8
    eq <- getPGFObj "="
    return $ t ||| strutX 0.3 ||| eq ||| strutX 0.3 ||| t'

derivProd = do
    objs <- mapM getPGFObj [
        "A \\times X \\stackrel{1_A \\times f}{\\to} {A \\times Y}"
        ,"X \\stackrel{f}{\\to} Y"
        ,"A \\times -"]
    let alga = 1*2
        f i = view (ix $ i-1) objs
        t = derivTree f 1 alga # centerXY ||| f 3 # scale 0.8
    return t

productUnitLaw1 = do
    objs <- mapM getPGFObj ["A","A \\times X" , "X","X","A \\times X", "A"]
    labs <- mapM getPGFLabel ["\\pi_1","\\pi_2","1_X","1_A \\times 1_X","1_A","\\pi_1","\\pi_2"]
    let l i = view (ix $ i - 1) labs
        alga = 5*(6+4+2) + (6+2)*1 + (2+4)*3
        trl = fromOffsets [unitX,unitX,unitY,unit_X,unit_X]
        update = tackLabel 2 1 (l 1) True . tackLabel 2 3 (l 2) False . tackLabel 4 3 (l 3) True . tackLabel_ 5 2 (l 4) True 0.5 0.2 . tackLabel 6 1 (l 5) False
                . tackLabel 5 6 (l 6) False . tackLabel 5 4 (l 7) True
        d = genDiagram trl objs update alga
    return d

productUnitLaw2 = do
    objs <- mapM getPGFObj ["A","A \\times X" , "X","X","A \\times X", "A"]
    labs <- mapM getPGFLabel ["\\pi_1","\\pi_2","1_X","1_{A \\times X}","1_A","\\pi_1","\\pi_2"]
    let l i = view (ix $ i - 1) labs
        alga = 5*(6+4+2) + (6+2)*1 + (2+4)*3
        trl = fromOffsets [unitX,unitX,unitY,unit_X,unit_X]
        update = tackLabel 2 1 (l 1) True . tackLabel 2 3 (l 2) False . tackLabel 4 3 (l 3) True . tackLabel_ 5 2 (l 4) True 0.5 0.2 . tackLabel 6 1 (l 5) False
                . tackLabel 5 6 (l 6) False . tackLabel 5 4 (l 7) True
        d = genDiagram trl objs update alga
    return d

productUnitLaw = do
    x <- productUnitLaw1
    y <- productUnitLaw2
    eq <- getPGFObj "="
    return $ hsep 0.1 [x,eq # translateY 0.5 ,y]

productCompositionLaw = do
    objs1 <- mapM getPGFObj ["A","A \\times Z","Z","Y","A \\times Y","A","A","A \\times X","X"]
    labs1 <- mapM getPGFLabel ["\\pi_1","\\pi_2","g","1_A \\times g","1_A","f","1_A \\times f"]
    let l xs i = view (ix $ i-1) xs
        l1 = l labs1
        trl1 = fromOffsets [unitX,unitX,unitY,unit_X,unit_X,unitY,unitX,unitX]
        alga1 = path [8,7,6,1] + path [8,9,4,3] + path [8,5,2,1] + 5*(6+4) + 2*3
        update1 = tackLabel 2 1 (l1 1) True . tackLabel 2 3 (l1 2) False . tackLabel 4 3 (l1 3) True . tackLabel_ 5 2 (l1 4) True 0.5 0.2 . tackLabel 6 1 (l1 5) False
                . tackLabel 6 5 (l1 1) False . tackLabel 5 4 (l1 2) True . tackLabel 7 6 (l1 5) False . tackLabel_ 8 5 (l1 7) True 0.5 0.2 . tackLabel 9 4 (l1 6) True
                . tackLabel 8 7 (l1 1) False . tackLabel 8 9 (l1 2) True . tackLabel 5 6 (l1 1) False
        d1 = genDiagram trl1 objs1 update1 alga1
    objs2 <- mapM getPGFObj ["A","A \\times Z","Z","X","A \\times X","A"]
    labs2 <- mapM getPGFLabel ["\\pi_1","\\pi_2","1_A","1_A \\times fg","fg"]
    let l2 = l labs2
        trl2 = fromOffsets [unitX,unitX,unitY,unit_X,unit_X]
        alga2 = 5*(2+4+6) + 6*1 + 4*3 + 2*(1+3)
        update2 = tackLabel 2 1 (l2 1) True . tackLabel 2 3 (l2 2) False . tackLabel 6 1 (l2 3) False . tackLabel_ 5 2 (l2 4) True 0.5 0.2 . tackLabel 4 3 (l2 5) True
                . tackLabel 5 6 (l2 1) False . tackLabel 5 4 (l2 2) True
        d2 = genDiagram trl2 objs2 update2 alga2
    return . hsep 0.3 $ map centerXY [d1,d2]



homContraVariant = do
    objs <- mapM getPGFObj ["X \\stackrel{f}{\\to} Y \\stackrel{\\varphi}{\\to} A","Y \\stackrel{\\varphi}{\\to} A"]
    let f i = view (ix $ i - 1) objs
        alga = 1*2
        t = derivTree f 1 alga # centerXY
    l <- getPGFObj "(f,A)"
    return $ hsep 0.05 [t,l]

genDerivation xs lab = do
    objs <- mapM getPGFObj xs :: OnlineTex [Diagram PGF]
    let f i = view (ix $ i - 1) objs
        alga = 1*2
        t = derivTree f 1 alga # centerXY
    l <- getPGFObj lab
    return $ hsep 0.05 [t,l] 

homContraUnit1 = genDerivation ["X \\stackrel{\\Box f}{\\to} X \\stackrel{\\psi}{\\to} A","X \\stackrel{\\psi}{\\to} A"] "(\\Box f,A)"

homContraUnit2 = genDerivation ["Y \\stackrel{f \\Box}{\\to} Y \\stackrel{\\varphi}{\\to} A","Y \\stackrel{\\varphi}{\\to} A"] "(f \\Box,A)"


-- 上の方のhomCovMorを参照すべし。
homContraComposition = do
    objs <- mapM getPGFObj ["X \\stackrel{f}{\\to} Y \\stackrel{g}{\\to} Z \\stackrel{\\varphi}{\\to} A","Y \\stackrel{g}{\\to} Z \\stackrel{\\varphi}{\\to} A","Z \\stackrel{\\varphi}{\\to} A","(g,A)","(f,A)","(fg,A)"]
    objs' <- mapM getPGFObj ["X \\stackrel{fg}{\\to} Z \\stackrel{\\varphi}{\\to} A","Z \\stackrel{\\varphi}{\\to} A"]
    let alga = 2*3
        f i = view (ix $ i - 1) objs
        g i = view (ix $ i - 1) objs'
        subt = derivTree f 2 alga # centerXY ||| f 4 # scale 0.8
        d = 1.01 * diameter unitX subt
        t = centerXY (subt === strutY 0.01 === (hrule d # lw veryThin ||| f 5 # scale 0.8) === strutY 0.01 === f 1)
        -- ここから二つめの導出図
        alga1 = 1*2
        t' = centerXY $ derivTree g 1 alga1 # centerXY ||| f 6 # scale 0.8
    eq <- getPGFObj "="
    return $ t ||| strutX 0.2 ||| eq ||| strutX 0.2 ||| t'


-- ======================= 図式と関手

template1 alga = do
    objs <- mapM getPGFObj $ zipWith (++) (replicate 6 "B_") (map show [1..6])
    labs <- mapM getPGFLabel ["f_1","f_2","f_3","f_4","f_5","f_6","f_7","f_8","f_9"]
    let trl = fromOffsets [unitX + unit_Y,unitX,unit_X + unit_Y, unitX,unitX+unit_Y]
        l i = view (ix $ i-1) labs
        -- objs = replicate 6 bc
        update = tackLabel 1 2 (l 1) True . tackLabel 2 4 (l 2) False . tackLabel 2 3 (l 3) True . tackLabel 4 5 (l 4) True . tackLabel 3 5 (l 5) False 
                . tackLabel 3 4 (l 6) False . tackLabel 4 6 (l 7) False . tackLabel 3 6 (l 8) True . tackLabel 5 6 (l 9) True
        d = genDiagram trl objs update alga
    return d

catB = do
    x <- getPGFObj "\\mathbf{B}:"
    y <- template1 $ (3+4+5)*6 + path [1,2,4,5] + 2*3 + 3*(4+5)
    return $ x ||| strutX 0.1 ||| y

subDiagram = do
    x <- getPGFObj "D:"
    y <- template1 $ path [2,3,4,5,6]
    return $ translate (unit_X + unitY) $ x # translate (unitX + unit_Y) ||| strutX 0.1 ||| y

freeCatD = do
    x <- getPGFObj "\\mathbf{D}:"
    y <- template1 $ path [2,3,4,5,6] + 2*4 + 3*(5+6) + 4*6
    return $ translate (unit_X + unitY) $ x # translate (unitX + unit_Y)  ||| strutX 0.1 ||| y

catBColored = do
    objs <- mapM getPGFObj $ zipWith (++) (replicate 6 "B_") (map show [1..6])
    labs <- mapM getPGFLabel ["f_1","f_2","f_3","f_4","f_5","f_6","f_7","f_8","f_9"]
    let trl = fromOffsets [unitX + unit_Y,unitX,unit_X + unit_Y, unitX,unitX+unit_Y]
        alga = (3+4+5)*6 + path [1,2,4,5] + 2*3 + 3*(4+5)
        l i = view (ix $ i-1) labs
        -- objs = replicate 6 bc
        redarr = over actions (lc red:)
        greenarr = over actions (lc green :)
        update = tackLabel 1 2 (l 1) True . tackLabel 2 4 (l 2) False . tackLabel 2 3 (l 3) True . tackLabel 4 5 (l 4) True . tackLabel 3 5 (l 5) False 
                . tackLabel 3 4 (l 6) False . tackLabel 4 6 (l 7) False . tackLabel 3 6 (l 8) True . tackLabel 5 6 (l 9) True
                . actOpt 2 3 (over actions (lc red :)) . actOpt 3 4 redarr . actOpt 4 5 redarr . actOpt 5 6 redarr 
                . actOpt 2 4 greenarr . actOpt 3 5 greenarr . actOpt 4 6 greenarr . actOpt 3 6 greenarr
        d = genDiagram trl objs update alga
    return d

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