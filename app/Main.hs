{-# LANGUAGE DeriveGeneric #-}
module Main where


import Diagrams.Prelude
import Parts
import DiagramLanguage
import PGFSurface
import ProofTree
import Algebra.Graph hiding(at,(===))
import Data.Yaml(decodeFileEither,ParseException)
import Data.Aeson
import Data.Either(fromRight)
import GHC.Generics
import GraphParser
import Data.Text (Text,pack)
import Data.Attoparsec.Text

data SizeOfImage = SI {
    widthOfI :: Int,
    heightOfI :: Int
}deriving(Show,Generic)

instance FromJSON SizeOfImage

data SimpleTree = ST{
    size :: SizeOfImage,
    nodes :: [String],
    structure :: Text
}deriving(Show,Generic)

--instance FromJSON a => FromJSON (Graph a)
instance FromJSON SimpleTree

--式から空白文字を除去するパーサー
rmSpace = do
    skipSpace
    x <- sepBy anyChar skipSpace
    skipSpace
    return x

main :: IO ()
main = do
    x' <- decodeFileEither "src/test.yaml" :: IO(Either ParseException SimpleTree)
    let x = fromRight (ST (SI 200 150) [] mempty) x'
        ns = nodes x
        objs = getFormula ns
        noSpaceExpr = pack . fromRight mempty . parseOnly rmSpace . structure $ x
        alga = fromRight Algebra.Graph.Empty . parseOnly expr $ noSpaceExpr
        -- w = widthOfI . size $ x
        -- h = heightOfI . size $ x
    renderPDF $  ((\x -> (genProofTree id x $ genTree 1 alga)) <$> objs)
