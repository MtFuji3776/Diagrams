module Char5 where

import Parts
import DiagramLanguage
import PGFSurface hiding (easyRender,renderPDF)
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))

easyRender name = PGFSurface.render $ "/Users/fujimotomakoto/haskell_testing/diagrams/src/References/DomainTheory" <> "/img/" <> name

-- renderPDF = "/Users/fujimotomakoto/haskell_testing/diagrams/src/References/DomainTheory"

homomorphism = do
    objs <- mapM getPGFObj ["A","A'","FA'","FA"]
    labs <- mapM getPGFLabel ["g","f'","Fg","f"]
    let alga = 4*(1+3) + (1+3)*2
        trl = fromOffsets [unitX,unitY,unit_X]
        l i = view (ix $ i-1) labs
        update = tackLabel 1 2 (l 1) False . tackLabel 3 2 (l 2) True . tackLabel 4 3 (l 3) True . tackLabel 4 1 (l 4) False
        d = genDiagram trl objs update alga
    return d

initialIsom = do
    objs <- mapM getPGFObj ["A","FA","A","FA","FFA","FA"]
    labs <- mapM getPGFLabel ["f","i","Fi","Ff"]
    let trl = fromOffsets [unitX,unitX,unitY,unit_X,unit_X] -- fromOffsetsを忘れるとPGFでNANが出力されてエラー
        alga = path [6,5,4,3] + path [6,1,2,3] + 5*2
        l i = view (ix $ i-1) labs
        update = tackLabel 1 2 (l 1) False . tackLabel 2 3 (l 2) False . tackLabel 4 3 (l 2) True . tackLabel 5 2 (l 3) False
                . tackLabel 6 1 (l 2) False . tackLabel 6 5 (l 4) True . tackLabel 5 4 (l 3) True
        d = genDiagram trl objs update alga
    return d

initialAlgebra = do
    objs <- mapM getPGFObj ["A","X","FX","FA"]
    labs <- mapM getPGFLabel ["\\overline{x}","x","F\\overline{x}","i"]
    let trl = fromOffsets [unitX,unitY,unit_X]
        alga1 = 4*1
        alga2 = alga1 + 3*2
        alga3 = alga2 + 4*3 + 1*2
        l i = view (ix $ i-1) labs
        update = tackLabel 1 2 (l 1) False . tackLabel 3 2 (l 2) True . tackLabel 4 3 (l 3) True . tackLabel 4 1 (l 4) False
        qs = [NoLine,Forall,ExistsOnly]
        ds = map (genDiagram trl objs update) [alga1,alga2,alga3]
    diagramLanguage qs ds