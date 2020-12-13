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