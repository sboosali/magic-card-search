{-# LANGUAGE NoImplicitPrelude #-}
module Card.Core where
import Card.Extra
import Card.Types

import Data.Aeson 

import Prelude.Spiros

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8 
import Paths_magic_card_search 

cardFile :: FilePath
cardFile = "data/SomeCards.json"

getCards :: IO B.ByteString
getCards = getDataFileName cardFile >>= B.readFile 

printCards = getCards >>= B8.putStrLn

parseCards cards = do 
   return cards 

