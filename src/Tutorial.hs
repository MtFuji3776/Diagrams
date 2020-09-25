{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts, TypeFamilies #-}
module Tutorial where

import Data.Typeable
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Size
import Diagrams.TwoD.Layout.Tree
import Data.Tree
import qualified Data.Set as Set


myCircle :: Diagram B
myCircle = circle 1

--easyRender :: (Show n, RealFloat n) => FilePath -> QDiagram SVG V2 n Any -> IO ()
easyRender path diag = renderPretty path fixedSize diag

setSize :: Num n => n -> n -> SizeSpec V2 n
setSize w h = mkSizeSpec2D (Just w) (Just h)

fixedSize :: Num n => SizeSpec V2 n
fixedSize = setSize 400 400


-- Algebraic Graph
data Graph = Empty | Vertex Int | Connect Graph Graph | Overlay Graph Graph deriving (Show)


instance Num Graph where
    fromInteger n = let m = fromInteger n in Vertex m
    (+) = Overlay
    (*) = Connect
    abs = id
    negate g = 
        case g of Tutorial.Empty -> Tutorial.Empty;
                  Vertex x -> Vertex (negate x);
                  Connect g1 g2 -> Connect (negate g1) (negate g2);
                  Overlay g1 g2 -> Overlay (negate g1) (negate g2)
    signum = id

toVertices :: Graph -> Set.Set Int
toVertices Tutorial.Empty = Set.empty
toVertices (Vertex n) = Set.singleton n
toVertices (Connect g1 g2) = Set.union (toVertices g1) (toVertices g2)
toVertices (Overlay g1 g2) = Set.union (toVertices g1) (toVertices g2)

toEdges :: Graph -> Set.Set (Int,Int)
toEdges Tutorial.Empty = Set.empty
toEdges (Vertex n) = Set.empty
toEdges (Overlay g1 g2) = Set.union (toEdges g1) (toEdges g2)
toEdges (Connect g1 g2) = Set.union (toEdges g1) (toEdges g2) <> Set.fromList [(v1,v2) | v1 <- Set.toList (toVertices g1),v2 <- Set.toList (toVertices g2)]

univProd :: (a -> b) -> (a -> c) -> a -> (b,c)
univProd f g x = (f x,g x)

destruct :: Graph -> ([Int],[(Int,Int)])
destruct = univProd (Set.toList . toVertices) (Set.toList . toEdges)

genRose :: Graph -> Int -> Tree Int
genRose Tutorial.Empty x = Node x []
genRose g x = let (vs,es) = destruct g; vs' = map snd $ filter (\t -> fst t == x) es in Node x $ map (genRose g) vs'

-- 以下、チュートリアルのexampleたち
main = mainWith myCircle

f = renderSVG "circle.svg" (mkSizeSpec2D (Just 1999) (Just 2000)) myCircle 

circle1 = circle 1 # fc blue # lw veryThick # lc purple # dashingG [0.2,0.05] 0

circle2 = circle 1 # fc red # lw none

squarecircle = square 1 # fc aqua `atop` circle 1

sidebyside = circle 1 ||| square 2

ontop = circle 1 === square 2

hcat_vcat = let circles = hcat (map circle [1..6]) in vcat $ replicate 3 circles --半径1~6の円が水平(horizontal)に6つ並んだものが、縦に3つ並んで描画される

