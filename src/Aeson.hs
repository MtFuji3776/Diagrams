module Aeson where

import Data.Aeson(encode,decode)
import qualified Data.ByteString.Lazy as BL (readFile,writeFile)
import qualified Data.Map as M (toList,empty,Map(..),singleton,fromList)
import Data.Maybe(fromMaybe)


genJson :: IO ()
genJson = do
    let j = M.fromList $ zip [1,2,3] [((2,1),"\\mathfrak{testP}"), ((-1,0),"test") ,((3,2),"\\mathfrak{O}")  ] :: M.Map Int ((Double,Double),String)
    BL.writeFile "data.json" $ encode j

getObjectData :: IO [(Int,((Double,Double),String))]
getObjectData = do
    bs <- BL.readFile "data.json"
    let mp' = decode bs :: Maybe (M.Map Int ((Double,Double),String))
        mp = fromMaybe M.empty mp'
        xs = M.toList mp
    return xs