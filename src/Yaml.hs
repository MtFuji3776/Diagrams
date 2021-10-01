{-# LANGUAGE DeriveGeneric #-}
module Yaml where

import GHC.Generics(Generic)
import Data.Yaml(decodeFileEither,ParseException)
import Data.Aeson(FromJSON,ToJSON)
import Data.Aeson.Types(parseEither)
import Data.Either(fromRight)
import Data.Text(Text)
import Algebra.Graph()
import Data.String(IsString,fromString)

instance FromJSON a => FromJSON(Graph a)

instance ToJSON a => ToJSON (Graph a) where
    toEncoding = genericToEncoding defaultOptions


data TwoDCoor = TDC{
    xCoor :: Double,
    yCoor :: Double
}deriving(Show,Generic)


instance ToJSON TwoDCoor where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON TwoDCoor

data Circle = Circle{
    twoDCoor :: TwoDCoor,
    radius :: Double
    }deriving(Show,Generic)

instance ToJSON Circle where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Circle

