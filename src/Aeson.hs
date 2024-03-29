{-# LANGUAGE DeriveGeneric #-}
module Aeson where

import Data.Aeson(encode,decode,Value(..),FromJSON,ToJSON)
import qualified Data.ByteString.Lazy as BL (readFile,writeFile,ByteString)
import qualified Data.Map as M (toList,empty,Map(..),singleton,fromList,mapKeys,lookup)
import Data.Maybe(fromMaybe)
import Data.String(fromString)
import GraphParser
import GHC.Generics
import Algebra.Graph(Graph(..),vertexSet,edgeSet)
import Data.Set(Set(..))
import Data.Text (Text)


-- type ObjectData = (Int,((Double,Double),String))
-- type MorphismData = ((Int,Int,Int),String)

data ObjectData
    = ObjData {
        idOfData      :: Int
    ,   coordinate    :: (Double,Double)
    ,   labelOfObject :: String
    } deriving(Show,Generic)

instance FromJSON ObjectData

data MorphismData
    = MorphData{
        idOfMorphism    :: (Int,Int,Int)
    ,   labelOfMorphism :: String
    -- ,   sideOfLabel     :: Bool
    ,   ratioOfLabel    :: Double
    ,   distanceOfLabel    :: Double
    ,   vshift :: Double
    ,   dash :: Double
    ,   dashwhite :: Double
    } deriving(Show,Generic)

instance FromJSON MorphismData  

genJson :: IO ()
genJson = do
    let j = M.fromList $ zip [1,2,3,4] [((2,1),"X"), ((-1,0),"Y") ,((3,2),"Z") ,((-1,1),"W")  ] :: M.Map Int ((Double,Double),String)
        j1 = M.fromList $ zip [(1,2,0),(1,3,0),(2,3,0),(4,3,0)] ["f","g","h","i"] :: M.Map (Int,Int,Int) String
        bs1 = fromMaybe Null $ decode $ encode j :: Value
        bs2 = fromMaybe Null $ decode $ encode j1 :: Value
        mp = M.fromList $ zip ["objects","morphisms"] [bs1,bs2] :: M.Map String Value
    BL.writeFile "data.json" $ encode mp

getObjectData :: IO [ObjectData] --[(Int,((Double,Double),String))]
getObjectData = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        val = fromMaybe Null $ M.lookup "objects" protomp
        mp' = fromMaybe M.empty . decode . encode $ val :: M.Map Int ((Double,Double),String)
        -- xs = fromMaybe [] mp'
        xs = M.toList mp'
        toObjectData (n,((n1,n2),str)) = ObjData n (n1,n2) str
    return $ map toObjectData xs 


getMorphismData :: IO [MorphismData]--[((Int,Int,Int),String)]
getMorphismData = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        bs' = fromMaybe Null . M.lookup "morphisms" $ protomp 
        -- mp'' = fromMaybe M.empty . decode . encode $ bs' :: M.Map String String
        -- mp' = M.mapKeys (\x -> read x :: [Int]) mp'' :: M.Map [Int] String
        -- mp  = fromMaybe M.empty . decode . encode $ mp' :: M.Map (Int,Int,Int) String
        md = fromMaybe [] $ decode . encode $ bs' :: [MorphismData]
    return md


getMacros :: IO String
getMacros = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        macros' = fromMaybe Null . M.lookup "macros" $ protomp
        macros = fromMaybe "" . decode . encode $ macros' :: String
    return macros

getContent :: IO String
getContent = do
    bs <- BL.readFile "data.json"
    let protomp  = fromMaybe M.empty $ decode bs :: M.Map String Value
        content' = fromMaybe Null . M.lookup "content" $ protomp
        content  = fromMaybe "" . decode . encode $ content' :: String
    return content

getDiagramType :: IO String
getDiagramType = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        diagramtype' = fromMaybe Null . M.lookup "diagramtype" $ protomp
        diagramtype  = fromMaybe "" . decode . encode $ diagramtype' :: String
    return diagramtype

getAlgas :: IO ([Set Int],[Set(Int,Int)])
getAlgas = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        algas'  = fromMaybe Null . M.lookup "algas" $ protomp
        algas   = map algaparse $ fromMaybe [] . decode . encode $ algas' :: [Graph Int]
        vers    = map vertexSet algas
        morphs  = map edgeSet algas
    return (vers,morphs)

getQuantifiers :: IO [String]
getQuantifiers = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        quantifiers' = fromMaybe Null . M.lookup "quantifiers" $ protomp
        quantifiers  = fromMaybe [] . decode . encode $ quantifiers' :: [String]
    return quantifiers

getNodes :: IO [String]
getNodes = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        nodes' = fromMaybe Null . M.lookup "nodes" $ protomp
        nodes = fromMaybe [] . decode . encode $ nodes' :: [String]
    return nodes

getAlga :: IO Text
getAlga = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        alga' = fromMaybe Null . M.lookup "alga" $ protomp
        alga = fromMaybe "0" . decode . encode $ alga' :: Text
    return alga