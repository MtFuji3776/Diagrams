{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts, TypeFamilies #-}
module DrawCD where

import Data.Typeable
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Size
import Diagrams.TwoD.Layout.Tree
import Data.Tree
import qualified Data.Set as Set
import Data.Time
import Diagrams.TwoD.Vector
import Tutorial


obj = circle 0.02 # fc black -- :: (TrailLike b, Transformable b, Typeable (N b) , HasStyle b, V b )

sqr = fromOffsets [unitX,unitY,-unitX,-unitY]

coor x y d = d # translateX x # translateY y

example1 = sqr <> mconcat (map (uncurry coor) [(0,0),(1,0),(1,1),(0,1)] <*> [obj]) <> square 1 -- :: Diagram B

