module Freyd1_9 where

import Parts
import DiagramLanguage
import PGFSurface hiding (easyRender,easyRender')
import Diagrams.Prelude
import Algebra.Graph hiding(at,(===))

easyRender name = PGFSurface.render $ "/Users/fujimotomakoto/haskell_testing/diagrams/src/References/Freyd_CatAll" <> "/img/" <> name

easyRender' w h name = PGFSurface.render' w h $ "/Users/fujimotomakoto/haskell_testing/diagrams/src/References/Freyd_CatAll" <> "/img/" <> name

-- ======================== 1.98

nnoDefinition = do
    objs <- mapM getPGFObj ["1","N","N","A","A"]
    labs <- mapM getPGFLabel ["0","s","a","t"]
    let trl = fromOffsets [unitX,unitX,unit_Y,unit_X]
        alga1 = path [1,2,3]
        alga2 = alga1 + path [1,5,4]
        alga3 = alga2 + 2*5 + 3*4
        l i = view (ix $ i - 1) labs
        ghost = place (circle 0.15 # lw none :: Diagram PGF) (0 ^& (-1))
        update = tackLabel 1 2 (l 1) True . tackLabel 2 3 (l 2 ) True . tackLabel 1 5 (l 3) False . tackLabel 5 4 (l 4) False
        ds = over (ix 0) (<> ghost) $ map (genDiagram trl objs update) [alga1,alga2,alga3]
        qs = [NoLine,Forall,ExistsOnly]
    diagramLanguage qs ds

-- ========1.981

primitiveRecursion = do
    objs <- mapM getPGFObj ["A","A \\times N","A \\times N","B","B"]
    labs <- mapM getPGFLabel ["\\langle 1_A,0 \\rangle","1_A \\times s","x","t"]
    let trl = fromOffsets [unitX,unitX,unit_Y,unit_X]
        alga1 = path [1,2,3] + path [1,5,4]
        alga2 = alga1 + 2*5 + 3*4
        l i = view (ix $ i - 1) labs
        update = tackLabel 1 2 (l 1) True . tackLabel 2 3 (l 2) True . tackLabel 1 5 (l 3) False . tackLabel 5 4 (l 4) False
        ds = map (genDiagram trl objs update) [alga1,alga2]
        qs = [Forall,ExistsOnly]
    diagramLanguage qs ds

nnoUniversality = do
    objs <- mapM getPGFObj ["1","N","N","B^A","B^A"]
    labs <- mapM getPGFLabel ["0","s","\\overline{x}","t^A"]
    let trl = fromOffsets [unitX,unitX,unit_Y,unit_X]
        alga1 = path [1,2,3] + path [1,5,4]
        alga2 = alga1 + 2*5 + 3*4
        l i = view (ix $ i - 1) labs
        update = tackLabel 1 2 (l 1) True . tackLabel 2 3 (l 2) True . tackLabel 1 5 (l 3) False . tackLabel 5 4 (l 4) False
        ds = map (genDiagram trl objs update) [alga2]
        qs = [ExistsOnly]
    diagramLanguage qs ds