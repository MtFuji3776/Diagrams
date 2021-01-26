module References.DomainTheory.Char3 where

import Parts
import DiagramLanguage
import PGFSurface hiding (easyRender,easyRender')
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))

easyRender' w h name = PGFSurface.render' w h  $ "/Users/fujimotomakoto/Documents/latexs/DailyStrategy/Work/DomainTheory" <> "/img/" <> name

easyRender = easyRender' 150 112.5

-- 3.1
idempotentSplit = do
    objs <- mapM (getPGFObj) ["D","Im(f)","D","Im(f)","D"]
    labs <- mapM getPGFLabel ["\\overline{f}","i","f","1"]
    let trl = fromOffsets [unitX,unit_Y,unitX,unit_Y]
        alga = path [1,2,3,4,5] + path [1,3,5] + 2*4
        l i = view (ix $ i - 1) labs
        update = actOpt 2 3 monic . actOpt 4 5 monic . actOpt 1 2 cover . actOpt 3 4 cover
                . tackLabel 1 2 (l 1) True . tackLabel 2 3 (l 2) True . tackLabel 1 3 (l 3) False . tackLabel 2 4 (l 4) True . tackLabel 3 4 (l 1) True
                . tackLabel 3 5 (l 3) False . tackLabel 4 5 (l 2) True
        d = genDiagram trl objs update alga
    return d

-- その他

retractOnFunctorCat = do
    objs <- mapM getPGFObj ["\\mathbf{A}","[\\mathbf{A},Set]","\\mathbf{A}"]
    labs <- mapM getPGFLabel ["\\text{よ}","\\bigsqcup^{\\uparrow}(-)","1_{\\mathbf{A}}"]
    let trl = fromOffsets [unitX,unit_Y]
        alga = 1*(2+3) + 2*3
        l i = view (ix $ i-1) labs
        update = tackLabel 1 2 (l 1) True . tackLabel_ 2 3 (l 2) True 0.5 0.2 . tackLabel 1 3 (l 3) False
        d = genDiagram trl objs update alga
    return d
