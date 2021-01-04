module Relation where

import Parts
import DiagramLanguage
import PGFSurface hiding (easyRender,easyRender')
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))

easyRender' w h name = PGFSurface.render' w h $ "/Users/fujimotomakoto/Documents/latexs/DailyStrategy/Work/SatoDrill/Relation/img/" <> name

easyRender = easyRender' 100 75

monicpairs = do
    objs_ <- mapM getPGFObj ["A","B"]
    txt1 <- getPGFText "はmonic pair"
    labs <- mapM getPGFLabel ["f","g"]
    let trl = fromOffsets [unitX + unitY,unitX + unit_Y]
        alga = 2*(1+3)
        l i = view (ix $ i - 1) labs
        o i = view (ix $ i - 1) objs_
        objs = [o 1,bc,o 2]
        update = tackLabel 2 1 (l 1) False . tackLabel 2 3 (l 2) True
        d = genDiagram trl objs update alga
        g = hcat . map centerXY $ [d,txt1]
    return g

monicpairmonic = do
    obj <- getPGFObj "A \\times B"
    lab <- getPGFLabel "\\langle f,g \\rangle"
    txt <- getPGFText "はmonic"
    let trl = fromOffsets [ unit_Y]
        alga = 1 * 2
        update = tackLabel_ 1 2 lab True 0.5 0.15
        d = genDiagram trl [bc,obj] update alga
        g = hcat . map centerXY $ [d,txt]
    return g

graph = do
    objs <- mapM getPGFObj ["A","G_f","B"]
    labs <- mapM getPGFLabel ["\\pi_1","\\pi_2","f"]
    let trl = fromOffsets [unitX + unitY, unitX + unit_Y]
        alga = 2*(1+3) + 1*3
        l i  = view (ix $ i - 1) labs
        update = actOpt 2 1 isom . tackLabel 2 1 (l 1) False . tackLabel 2 3 (l 2) True . tackLabel 1 3 (l 3) False
        d = genDiagram trl objs update alga
    return d

canonicalGraph = do
    objs <- mapM getPGFObj ["A","A","B"]
    labs <- mapM getPGFLabel ["1_A","f","f"]
    let trl = fromOffsets [unitX + unitY, unitX + unit_Y]
        alga = 2*(1+3) + 1*3
        l i  = view (ix $ i - 1) labs
        update = actOpt 2 1 isom . tackLabel 2 1 (l 1) False . tackLabel 2 3 (l 2) True . tackLabel 1 3 (l 3) False
        d = genDiagram trl objs update alga
    return d

composedGraphs = do
    objs <- mapM getPGFObj ["A","G_f","G_{g \\circ f}","G_g","B","C"]
    labs <- mapM getPGFLabel ["\\pi_1","\\pi_2","f","l","r","\\pi_1'","\\pi_2'","g"]
    let trl = fromOffsets [1^&1,1^&1,unitX + unit_Y,unit_X + unit_Y,2 *^ unitX]
        alga = path [3,2,1,5,6] + path [3,4,6] + (2+4)*5
        l i = view (ix $ i - 1) labs
        update = actOpt 2 1 isom . actOpt 3 2 monic . actOpt 4 5 isom
                . tackLabel 2 1 (l 1) False . tackLabel 2 5 (l 2) True . tackLabel 1 5 (l 3 )False . tackLabel 3 2 (l 4) False
                . tackLabel 3 4 (l 5) True . tackLabel 4 5 (l 6) False . tackLabel 4 6 (l 7) True . tackLabel 5 6 (l 8) False
        d = genDiagram trl objs update alga
        plb' = place (rotate (-1/8 * tau @@ rad) plb) (7/4 *^ (unitX + unitY))
    return $ d <> plb'