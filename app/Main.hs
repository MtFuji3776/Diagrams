{-# LANGUAGE DeriveGeneric #-}
module Main where


--import Diagrams.Prelude()
import Parts(genTree,genBCDia)
-- import DiagramLanguage()
import PGFSurface(renderPDF,renderTex,renderTexWithMacro)
import ProofTree(getFormula,genProofTree)
import DiagramLanguage(Quantification,genDiagram,genDiagramFromJson,readQuant,diagramLanguage)
import Algebra.Graph(Graph(..)) -- hiding(at,(===))
import Data.Yaml(decodeFileEither,ParseException)
import Data.Aeson(FromJSON,decode,ToJSON,encode)
import Data.Either(fromRight)
import Data.Maybe(fromMaybe)
import GHC.Generics
import GraphParser
import Aeson
import Data.Text (Text,pack)
import Data.Attoparsec.Text(skipSpace,parseOnly,anyChar,sepBy)
import qualified Data.ByteString.Lazy as BL(readFile)
import Data.Set(member)
import Control.Monad.IO.Class (liftIO)
import Control.Monad(join)

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

-- diagrams用のデータ型一覧
data Object = OB{
     labelObj :: Text
    ,xCoor :: Double
    ,yCoor :: Double
}deriving(Show,Generic)

instance FromJSON Object

data ArrowType = Monic | Epic | Cover | Isom deriving(Show,Generic)

instance ToJSON ArrowType
instance FromJSON ArrowType

data Arrow = Arr{
    labelArr :: Text
    ,locatePath :: Double
    ,locateVertical :: Double
    ,arrowType :: ArrowType
}deriving(Show,Generic)

instance FromJSON Arrow

data SingleDiagram = SD
    {
        objects :: [Object]
    ,   arrows :: [Arrow]
    ,   structureOfDiagram :: Text
    }deriving(Show,Generic)

instance FromJSON SingleDiagram

data Diagrams = Ds 
    {
        diagrams :: [SingleDiagram]
    ,   quantifiers :: [Quantification]
    }

--式から空白文字を除去するパーサー
rmSpace = do
    skipSpace
    x <- sepBy anyChar skipSpace
    skipSpace
    return x

main :: IO ()
main = do
    --x' <- decodeFileEither "src/test.yaml" :: IO(Either ParseException SimpleTree)
    -- y <- BL.readFile "data.json"
    -- let y' = decode y :: Maybe SimpleTree
    -- let x = fromMaybe (ST (SI 200 150) [] mempty) y' -- ここもメタデータから取得できるようにするべし
    c <- getContent
    if c == "CommutativeDiagram"
    then do
        t <- getDiagramType
        if t == "single diagram"
            then do
                objs <- getObjectData
                morphs <- getMorphismData
                macros <- getMacros
                print objs
                print morphs
                renderTexWithMacro macros $ genDiagramFromJson objs morphs
            else do
                qs' <- getQuantifiers
                algas <- getAlgas
                objs <- getObjectData
                morphs <- getMorphismData
                macros <- getMacros
                print objs
                print morphs
                let filterobjsFromSet s = filter (\o -> member (idOfData o) s) objs
                    proj (i,j,k) = (i,j)
                    filterMorphsFromSet s = filter (\m -> member (proj $ idOfMorphism m) s) morphs
                    filteredObjs = map filterobjsFromSet $ fst algas :: [[ObjectData]]
                    filteredMorphs = map filterMorphsFromSet $ snd algas :: [[MorphismData]]
                    qs = map readQuant qs' 
                    ds = sequence $ zipWith genDiagramFromJson filteredObjs filteredMorphs
                renderTexWithMacro macros . join $ diagramLanguage qs <$> ds -- diagramLanguageの戻り値はOnlineTex (Diagram PGF)であることに注意。
    else if c == "ProofTree"
    then do
        nodes  <- getNodes
        alga'  <- getAlga
        macros <- getMacros
        -- objs   <- getFormula nodes
        let alga = fromRight Algebra.Graph.Empty . parseOnly expr $ alga'
            tree = genTree 1 alga
        renderTexWithMacro macros $ (\x -> genProofTree id x tree) <$> getFormula nodes
    else return ()
        
    --     ns = nodes x
    --     objs = getFormula ns
    --     noSpaceExpr = pack . fromRight mempty . parseOnly rmSpace . structure $ x
    --     alga = fromRight Algebra.Graph.Empty . parseOnly expr $ noSpaceExpr
    --     -- w = widthOfI . size $ x
    --     -- h = heightOfI . size $ x
    -- renderTex $  ((\x -> (genProofTree id x $ genTree 1 alga)) <$> objs)
