module Project.Drill.Functors where

import Parts
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))
import DiagramLanguage
import Diagrams.Backend.PGF
import PGFSurface

-- fig:~~毎に分類して保管していく


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

-- ==================================問の証明概要図=====================================================

q3 = do 
    f2 <- getPGFObj "\\forall f \\in \\mathbf{A} (A \\stackrel{f}{\\to} B \\iff B \\stackrel{f}{\\to} A \\text{ in } \\mathbf{A}^{op})"
    f3 <- getPGFObj "\\mathrm{Id}:\\mathbf{A} \\to \\mathbf{A}^{op}"
    f4 <- getPGFObj "\\mathrm{Id}:\\mathbf{A}^{op} \\to \\mathbf{A}"
    return $ vsep 0.2 [f2,f3,f4] # pad 1.2
