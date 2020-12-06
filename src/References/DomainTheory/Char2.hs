module References.DomainTheory.Char2 where

import Parts
import DiagramLanguage
import Diagrams.Prelude
import PGFSurface
import Algebra.Graph hiding(at,(===))

-- =============基本記号たち=================

symbols = [directed,subset,sup,closedSet]

-- 有向集合は折れ線
directed =
    let half = [unitX + 2 *^ unitY]
    in fromOffsets $ half ++ (map (scaleY (-1)) half)

-- 部分集合は基本ただの楕円
subset = circle 1 # scaleY 0.7

-- 上限を表すバッテン
sup = 
    let xs = [unitX + unitY]
    in scale 0.1 $ fromOffsets xs # centerXY <> fromOffsets (map (scaleY (-1)) xs) # centerXY :: Diagram PGF

-- Scott閉集合
closedSet =
    let ps = fromOffsets [unitX + 2 *^ unitY,unitX + 2 *^ unit_Y]
        frame = arrowFromLocatedTrail' (def & arrowTail .~ dart') $ cubicSpline False ps
    in frame

-- =================基本概念たち=============

diaSup =
    let d1 = directed # centerXY # named "1" <> place bc (p2 (1.5,2.5)) # named "2" 
            <> place sup (p2 (1,2)) # named "3" 
            # connectOutside "1" "2"
        d2 = d1 # connectOutside "2" "3"
        qs = [Forall,Exists]
    in diagramLanguage qs [d1,d2]

defOfClosedSet = do
    txt <- mapM getPGFText ["\\textbf{Scott閉集合}",":任意の有向部分集合が上限で閉じている"]
    let p  = p2 (1.2 , 1.9)
        d1 = closedSet <> place (directed # scaleX 0.7 # centerX # alignT) p
        d2 = d1 <> place sup p
        qs = [NoLine,Forall,Exists]
        t i = view (ix (i-1)) txt
    dl <- diagramLanguage qs [closedSet,d1,d2]
    return $ vsep 0.1 [(t 1 ||| t 2) # scale 2 # centerXY ,dl # centerXY]

-- ===================特殊な順序集合たち=========================

dcpo = 
    let d1 = directed
        d2 = directed <> place sup (1 ^& 2)
        qs = [Forall,Exists]
    in diagramLanguage qs [d1,d2] <&> pad 1.1

contDomain = do
    labs <- mapM getPGFObj ["x" -- 1
                             ,"\\downarrow x"
                             ,"B \\cap \\downarrow x"] -- 3
    txts <- getPGFLabel "・\\text{任意の点$x$のlower set$\\downarrow x$に対して、Basisとの交叉が$x$上限の有向集合になる}"
    let l i = view (ix (i-1)) labs
        d1 = directed <> place (l 2) (1 ^& 0.3) <> place (l 1) (1.1 ^& 2) <> place sup (1 ^& 2)
        d2 = directed <> place (l 1) (1.1 ^& 2) <> place sup (1 ^& 2)
            <> place (directed # scaleX 0.7 # centerXY) (1 ^& 1) <> place (l 3) (1 ^& 0.3)
        qs = [Forall,Exists]
    dl <- diagramLanguage qs [d1,d2] 
    return $　pad 1.1 $ dl # centerXY === strutY 0.2 === txts 

