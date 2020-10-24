module AbacusDesign where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Attributes
import Parts


-- そろばんの珠
    -- 一筆書きで描いてしまうとfillTextureが面倒かも
beed = 
    let xs = [0.8 ^& 0.6, 0.4 ^& 0 , 0.8 ^& (-0.6) , (-2)^&0] :: [V2 Double]
        ys = map (scaleY (-1)) xs
    in  mconcat . map (stroke . glueLine . fromOffsets) $ [xs,ys]

-- そろばんの珠（半分バージョン）
halfBeed = strokeLoop . glueLine $ fromOffsets [8 ^& 6 , 4 ^& 0, 8 ^& (-6) , (-20) ^& 0] # scale 0.1


beedflash = mkLinearGradient (mkStops [(brown,0,1),(white,1,2)])
                             (0.3 *^ unitY)
                             origin 
                             GradPad

-- なんかうまく調節できない。
beedFlashR = mkRadialGradient (mkStops [(brown,0,1),(white,1,1)])
                              ((0.9)^&0.05) 
                              2 
                              (1^&0) 
                              0.01
                              GradPad

coloredBeed = halfBeed # lw veryThin # fillTexture beedflash
            <> halfBeed # scaleY (-1) # fc (blend 0.5 brown black)

coloredBeedR = halfBeed # lw veryThin # fillTexture beedFlashR
            <> halfBeed # scaleY (-1) # fc (blend 0.5 brown black)

-- Radial Gradientの練習
radial = mkRadialGradient (mkStops [(white,0,1) , (black , 1,1)]) 
                          ((-0.1) ^& 0.3) 0.06 (0 ^& 0) 0.5
                          GradPad
linear = mkLinearGradient (mkStops [(black,0,1),(white,1,1)])
                          (0 ^& (-0.5)) (0^&0.5)
                          GradPad
example = circle 0.35 # fillTexture radial # lw none
        <> rect 2 1 # lw none # fillTexture linear
