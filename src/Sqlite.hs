{-# LANGUAGE OverloadedStrings,QuasiQuotes #-}
module Sqlite() where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Database.SQLite.Simple.FromRow
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

data TestField = TestField Int Text deriving(Show)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

instance ToRow TestField where
    toRow (TestField id_ str) = toRow (id_,str)

data DiagramWork = DiagramWork {
                                idNumber :: Int
                               ,textData :: Text
                               ,pdfData  :: ByteString
                               ,titleOfDiagram :: Text
                               ,dateCreated :: Text
                               ,dateLastUpdate :: Text
                               }
    deriving (Show)

instance FromRow DiagramWork where
    fromRow = DiagramWork <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DiagramWork where
    toRow (DiagramWork id_ txt pdf title dateC dateLU) = toRow (id_,txt,pdf,title,dateC,dateLU)

dbFilePath :: String
dbFilePath = "/Users/fujimotomakoto/haskell_testing/diagrams/src/test.db"

test :: IO ()
test = do
    conn <- open "/Users/fujimotomakoto/haskell_testing/diagrams/src/test.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
    execute_ conn "INSERT INTO test (str) VALUES (datetime('now','localtime'))" 
    --execute conn "INSERT INTO test (id, str) VALUES (?,datetime('now','localtime'))" (Only (11 :: Int))
    rowID <- (\x -> x-10) <$> lastInsertRowId conn
    executeNamed conn "UPDATE test Set str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowID]
    r <- query_ conn "SELECT * FROM test" :: IO [TestField]
    mapM_ print r
    execute conn "DELETE FROM test WHERE id = ?" (Only rowID)
    close conn

test_ :: IO ()
test_ = do
    conn <- open "/Users/fujimotomakoto/haskell_testing/diagrams/src/test.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS test_ (idNumber INTEGER PRIMARY KEY, textData TEXT,pdfData BLOB,titleOfDiagram TEXT,dateCreated Text,dateLastUpdate Text)"
    pdf <- C.readFile "test1.pdf"
    tex <- T.pack <$> (readFile "test.tex")
    execute conn "INSERT INTO test_ (textData,pdfData,titleOfDiagram,dateCreated,dateLastUpdate) VALUES (?,?,'notitle',datetime('now','localtime'),'unset')" ((tex,pdf) :: (Text,ByteString) )
    --r <- query_ conn "SELECT * FROM test_" :: IO [DiagramWork]
    --mapM_ print r
    close conn


