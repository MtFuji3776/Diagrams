module References.Freyd_CatAll.AppendB() where

import Parts
import DiagramLanguage
import Diagrams.Prelude
import ProofTree
import PGFSurface hiding(easyRender)
import Algebra.Graph hiding(at,(===))

easyRender name = PGFSurface.render ("/Users/fujimotomakoto/haskell_testing/diagrams/src/references/freyd_catall/img/" ++ name)

-- proofTree関数側で線の種類を選べるようにする必要ありか。
    -- ラベル付与と同様に、RoseTreeの各ノードにフラグを持たせる仕様にすれば良いか。
doubleLine d = hrule d === strutY 0.01 === hrule d


-- ===========================B.2.

oneOne objs =
    let alga = 2*1
        t = genTree 2 alga
        l i = view (ix $ i-1) objs
        pt = proofTree . formulaTree l $ t
    in return pt

twoOne objs =
    let alga = 3*(1+2)
        t = genTree 3 alga
        l i = view (ix $ i-1) objs
        pt = proofTree $ formulaTree l t
    in return pt

transit = do
    objs <- mapM getPGFObj ["A \\rightharpoonup B","B \\rightharpoonup C","A \\rightharpoonup C"]
    let alga = 3*(1+2)
        t = genTree 3 alga
        l i = view (ix $ i-1) objs
        pt = proofTree $ formulaTree l t
    return pt

productUniv = do
    objs <- mapM getPGFObj ["A \\rightharpoonup B", "A \\rightharpoonup C","A \\rightharpoonup B \\land C"]
    twoOne objs

coprodUniv = do
    objs <- mapM getPGFObj ["A \\rightharpoonup B", "A \\rightharpoonup C","A \\lor B \\rightharpoonup C"]
    twoOne objs

