module Freyd2_1() where

import Diagrams.Prelude
import Parts
import DiagramLanguage
import Algebra.Graph hiding(at,(===))
import PGFSurface hiding(easyRender)

easyRender name = PGFSurface.render ("/Users/fujimotomakoto/haskell_testing/diagrams/src/References/Freyd_CatAll/img/" ++ name)

-- ========================== 2.1

nonmodular = 
    let trl = fromOffsets $ map (0.5 *^) [unitX + unitY,unitY,unit_X + unitY,unit_X + 1.5 *^ unit_Y]
        objs = replicate 5 bc
        alga = path [1,2,3,4] + path [1,5,4]
    in return $ genDiagram trl objs id alga


-- ========================2.14



-- ========= 2.147

pullback2147 = do
    labs <- mapM getPGFLabel ["f","g"]
    let objs = replicate 3 bc
        alga = (1+3)*2
        trl = fromOffsets [unitX + unit_Y,unitX + unitY]
        l i = view (ix $ i - 1) labs
        update = tackLabel 1 2 (l 1) False . tackLabel 3 2 (l 2) True
        d = genDiagram trl objs update alga
    return d

maps2147 = do
    os <- mapM getPGFObj ["f:","g^\\circ :"]
    labs1 <- mapM getPGFLabel ["1","f"]
    labs2 <- mapM getPGFLabel ["g","1"]
    let trl = fromOffsets [unitX + unitY,unitX + unit_Y]
        alga = 2*(1+3)
        update f = tackLabel 2 1 (f 1) False . tackLabel 2 3 (f 2) True
        objs = replicate 3 bc
        l ls i = view (ix $ i - 1) ls
        l1 = l labs1
        l2 = l labs2
        d1 = genDiagram trl objs (update l1) alga
        d2 = genDiagram trl objs (update l2) alga
        o i = view (ix $ i - 1) os
        d = hcat $ map centerXY [o 1, d1, strutX 0.6, o 2,d2]
    return d

pullbackTabulation2147 = do
    obj <- getPGFObj "l^\\circ r"
    labs <- mapM getPGFLabel ["1","l","f","r","g","1"]
    let trl = fromOffsets [1 ^& 1,1^&1,1 ^& (-1),(-1) ^& (-1), 2 ^& 0]
        alga = path [3,2,1] + path [3,4,6] + (2+4)*5
        l i = view (ix $ i - 1) labs
        update = tackLabel 2 1 (l 1) False . tackLabel 3 2 (l 2) False . tackLabel 2 5 (l 3) True . tackLabel 3 4 (l 4) True . tackLabel 4 5 (l 5) False . tackLabel 4 6 (l 6) True
        objs = [bc,bc,obj] ++ replicate 3 bc
        d = genDiagram trl objs update alga
    return d