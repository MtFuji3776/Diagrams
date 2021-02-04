{-# LANGUAGE QuasiQuotes #-}
module Diary.Before0110() where

import Diagrams.Prelude hiding(star)
import Parts
import DiagramLanguage
import ProofTree
import PGFSurface
import Algebra.Graph hiding((===),at)
import Text.RawString.QQ

derive1 = do
    objs <- getFormula ["A","B","C","D"]
    let alga = star 1 [2,3,4]
        t = genProofTree id objs (genTree 1 alga)
        p1 = place bc (0.125 ^& (-0.14))
        --p2 = origin
        p3 = place bc $ (-0.125) ^& (-0.04)
        p4 = place bc $ (-0.4) ^& (-0.04)
        p5 = place bc $ (-0.4) ^& 0.18
        x1 = -0.4
        x2 = -0.125
        x3 = 0.125
        x4 = 0.4
        y1 = 0.18
        y2 = -0.04
        y3 = -0.15
        ps = zipWith (curry p2) [x1,x1,x2,x2,x3,x3,x4,x4,x1] [y1,y2,y2,y3,y3,y2,y2,y1,y1]
        dotted = fromVertices ps # dashed
    return $ t <> dotted

absord = do
    let f ass as [] = ass ++ [as]
        f ass as (x:xs) = if x == '\n' then f (ass ++ [as]) [] xs
                                       else f ass (as ++ [x]) xs
    objs <- getFormula $ f [] []
        [r|\lnot A \land P \rightharpoonup \bot
        P \rightharpoonup \lnot A \Rightarrow \bot (\equiv \lnot \lnot A)|]
    let alga = 2*1
        t = genProofTree id objs (genTree 2 alga)
    return t

levelOfMonic = do
    objs <- mapM getPGFObj ["A","A","ff^{\\circ}","A","A","B","A"]
    labs <- mapM getPGFLabel ["1","l","f","\\Delta_A"]
    let l i = view (ix $ i-1) labs
        ur = unitX + unitY
        vers = fromOffsets $ map (0.8 *^) [ur , ur , unitY, 2 *^ unit_Y + unitX,unit_X + unit_Y,2 *^ unitX]
        update = actOpt 2 6 monic . actOpt 5 6 monic . actOpt 3 2 monic . actOpt 3 5 monic . tackLabel 2 1  (l 1) False
            . tackLabel 3 2 (l 2) False . tackLabel 4 2  (l 1) False . tackLabel 3 5 (l 2) True . tackLabel 2 6  (l 3) False
            . tackLabel 5 6 (l 3) True . tackLabel 4 5  (l 1) True . tackLabel 5 7 (l 1) True . tackLabel 4 3 (l 4) True
        alga = 4*(2+3+5) + 3*(2+5) + 2*(1+6) + 5*(6+7)
        d = genDiagram vers objs update alga
    return d

simpleLevel = do
    objs <- mapM getPGFObj ["ff^{\\circ}","A","A","A"]
    labs <- mapM getPGFLabel ["l","1"]
    let l i = view (ix $ i - 1) labs
        trl = fromOffsets [unit_Y,unit_X + 0.5 *^ unit_Y,2 *^ unitX]
        alga = 1*(2+3+4) + 2*(3+4)
        update = actOpt 1 2 isom . actOpt 1 3 isom . actOpt 1 4 isom . tackLabel 1 3 (l 1) False . tackLabel 1 4 (l 1) True . tackLabel 2 3 (l 2) False 
                . tackLabel 2 4 (l 2) True
    return $ genDiagram trl objs update alga