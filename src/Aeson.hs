module Aeson where

import Data.Aeson(encode,decode,Value(..))
import qualified Data.ByteString.Lazy as BL (readFile,writeFile,ByteString)
import qualified Data.Map as M (toList,empty,Map(..),singleton,fromList,mapKeys,lookup)
import Data.Maybe(fromMaybe)
import Data.String(fromString)


type ObjectData = (Int,((Double,Double),String))
type MorphismData = ((Int,Int,Int),String)

genJson :: IO ()
genJson = do
    let j = M.fromList $ zip [1,2,3] [((2,1),"X"), ((-1,0),"Y") ,((3,2),"Z")  ] :: M.Map Int ((Double,Double),String)
        j1 = M.fromList $ zip [(1,2,0),(1,3,0),(2,3,0)] ["f","g","h"] :: M.Map (Int,Int,Int) String
        bs1 = fromMaybe Null $ decode $ encode j :: Value
        bs2 = fromMaybe Null $ decode $ encode j1 :: Value
        mp = M.fromList $ zip ["objects","morphisms"] [bs1,bs2] :: M.Map String Value
    BL.writeFile "data.json" $ encode mp

getObjectData :: IO [(Int,((Double,Double),String))]
getObjectData = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        val = fromMaybe Null $ M.lookup "objects" protomp
        mp' = decode . encode $ val :: Maybe (M.Map Int ((Double,Double),String))
        mp = fromMaybe M.empty mp'
        xs = M.toList mp
    return xs

getMorphismData :: IO [((Int,Int,Int),String)]
getMorphismData = do
    bs <- BL.readFile "data.json"
    let protomp = fromMaybe M.empty $ decode bs :: M.Map String Value
        bs' = fromMaybe Null . M.lookup "morphisms" $ protomp 
        mp' = fromMaybe M.empty . decode . encode $ bs' :: M.Map (Int,Int,Int) String
        --mp = M.mapKeys (fromMaybe (0,0,0) . decode . fromString) mp'
    return . M.toList $ mp'