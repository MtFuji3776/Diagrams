module AbacusDesign where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Vector
import Parts


-- そろばんの珠
beed = 
    let xs = [0.7 ^& 0.5, 0.6 ^& 0 , 0.7 ^& (-0.5)] :: [V2 Double]
        ys = map negate xs
    in fromOffsets $ xs ++ ys ++ [2.0 ^& 0]
