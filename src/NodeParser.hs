{-# LANGUAGE DeriveGeneric #-}

module NodeParser (parseFile) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.KeyMap (toMap, KeyMap)
import GHC.Generics
import qualified Data.Map as Map

data ParsedNodes = ParsedNodes { nodes :: Map.Map String Int } deriving (Show, Generic)

instance FromJSON ParsedNodes

parseFile :: FilePath -> IO (Either String ParsedNodes)
parseFile file = do
    contents <- B.readFile file
    return (eitherDecode contents)
