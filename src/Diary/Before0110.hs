{-# LANGUAGE QuasiQuotes #-}
module Before0110() where

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