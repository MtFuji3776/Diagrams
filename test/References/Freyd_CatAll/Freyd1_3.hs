module References.Freyd_CatAll.Freyd1_3 where

import Parts
import DiagramLanguage
import PGFSurface hiding(easyRender)
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))

easyRender name = PGFSurface.render ("/Users/fujimotomakoto/haskell_testing/diagrams/src/References/freyd_catall/img/" <> name)


-- ======================== 1.36

kernelInflation = do
    objs <- mapM getPGFObj ["A","A'","Y","X"]
    labs <- mapM getPGFLabel ["k","x","k'","kxk'"]
    let trl = fromOffsets [unitX,unitY,unit_X]
        alga = 4*(1+3) + (1+3)*2
        l i = view (ix $ i-1) labs
        update = tackLabel 1 2 (l 2) False
               . tackLabel 4 1 (l 1) False
               . tackLabel 3 2 (l 3) True
               . tackLabel 4 3 (l 4) True
    return $ genDiagram trl objs update alga

kerInfCompose = do
    objs <- mapM getPGFObj["A","A'","B","B'","Z","Y","X"]
    labs <- mapM getPGFLabel ["x" -- 1
            ,"k'l"
            ,"y"  -- 3
            ,"l'"
            ,"lyl'" -- 5
            ,"kxk'"
            ,"k"  -- 7
            ,"k'"
            ,"l"] -- 9
    let l i = view (ix $ i - 1) labs
        k = 1+ (1/sqrt 3)
        trl = fromOffsets [unitX,(2 / sqrt 3) *^ unitX , unitX,unitY, k *^ unit_X, k *^ unit_X]
        alga = path [1,2,3,4,5] + path [2,6,3] + path [7,6,5] + 7*1
        update = tackLabel 1 2 (l 1) False
                . tackLabel 2 3 (l 2) False
                . tackLabel 3 4 (l 3) False
                . tackLabel 4 5 (l 4) False
                . tackLabel 7 1 (l 7) False
                . tackLabel 7 6 (l 6) True
                . tackLabel 6 5 (l 5) True
                . tackLabel 2 6 (l 8) False
                . tackLabel 6 3 (l 9) False
    return $ genDiagram trl objs update alga

kerInfUniv = do
    objs <- mapM getPGFObj ["\\mathbf{A}","\\mathbf{A}/\\mathcal{K}","\\mathbf{B}"]
    lab <- getPGFLabel "F"
    let trl = fromOffsets [unitY,unitX]
        alga1 = 1*(2+3)
        alga2 = 1*(2+3) + 2*3
        update = tackLabel 1 3 lab False
        qs = [Forall,ExistsOnly]
        ds = map (genDiagram trl objs update) [alga1,alga2]
    diagramLanguage qs ds 