{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
{-# LANGUAGE LambdaCase, OverloadedStrings #-} 

module Card.Example where
import Card
import System.Environment
-- import Data.Function((&)) 
-- import qualified Data.Map as M 
-- import qualified Data.Text as T
-- import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8 
-- import Prelude (head) 
-- import Control.Lens hiding ((<&>)) 
-- import Data.Aeson
-- import Data.Aeson.Lens
-- import qualified Data.HashMap.Strict as H 
-- import Data.HashMap.Strict  (HashMap) 
-- import Data.Maybe 
-- import qualified Data.Vector as V 
-- import Control.Arrow((>>>)) 


printAllSets = getSetsDefault >>= B8.putStrLn

printAllCards = getCardsDefault >>= B8.putStrLn


{-|
@
stack build && stack exec -- example-magic-card-search
@

an example with a single card: 

@
{"Coiling Oracle":{"layout":"normal","name":"Coiling Oracle","manaCost":"{G}{U}","cmc":2,"colors":["Blue","Green"],"type":"Creature — Snake Elf Druid","types":["Creature"],"subtypes":["Snake","Elf","Druid"],"text":"When Coiling Oracle enters the battlefield, reveal the top card of your library. If it's a land card, put it onto the battlefield. Otherwise, put that card into your hand.","power":"1","toughness":"1","imageName":"coiling oracle","colorIdentity":["U","G"]}, ...}
@

which is pretty printed more readably as: 

@
{ "Coiling Oracle" : {
    "layout" : normal,
    "name" : Coiling Oracle,
    "manaCost" : {G}{U},
    "cmc" : 2,
    "colors" : [
        Blue,
        Green
        ],
    "type" : Creature — Snake Elf Druid,
    "types" : [
        Creature
        ],
    "subtypes" : [
        Snake,
        Elf,
        Druid
        ],
    "text" : When Coiling Oracle enters the battlefield, reveal the top card of your library. If it's a land card, put it onto the battlefield. Otherwise, put that card into your hand.,
    "power" : 1,
    "toughness" : 1,
    "imageName" : coiling oracle,
    "colorIdentity" : [
        U,
        G
        ]
    }
}
@

some of these fields can be derived from others, so we can compress this into: 

@
{ "Coiling Oracle" : {
    "layout" : normal,
    "name" : Coiling Oracle,
    "manaCost" : {G}{U},
    "type" : Creature — Snake Elf Druid,
    "text" : When Coiling Oracle enters the battlefield, reveal the top card of your library. If it's a land card, put it onto the battlefield. Otherwise, put that card into your hand.,
    "power" : 1,
    "toughness" : 1,
    }
}
@ 

or with slightly tighter invariants as: 

@
{ "Coiling Oracle" : {
    "layout" : normal,
    "name" : Coiling Oracle,
    "cost" : {G}{U},
    "type" : Creature — Snake Elf Druid,
    "text" : When Coiling Oracle enters the battlefield, reveal the top card of your library. If it's a land card, put it onto the battlefield. Otherwise, put that card into your hand.,
    "statistics" : [1, 1],
    }
}
@ 


-}
main :: IO ()
main = do
 arguments <- getArgs >>= \case
  [s] -> return (s)
  _ -> return ("")
 mainWith arguments

someMCICardIdentifiers = fmap (defaultMCICardIdentifier "xln") [24..284 ] -- [1..21] -- [1..284] 
-- TODO some, like double-faced cards, don't have numerical collector numbers. for example, there is no 22, only 22a and 22b
-- get the real collectors numbers from the file  

mainWith _ = do
    putStrLn "" 

    -- print $ exampleDecoding1 
    -- print $ exampleDecoding2 

--  bc <- getOneCard 
--  putStrLn ""
--  let c :: Either String CardObject = eitherDecode' bc
--  print c 

--  bs <- getSetsDefault -- getOneSet 
--  putStrLn ""
--  let s :: Either String SetsObject = eitherDecode' bs
--  print s

--  s <- getSets  
--  putStrLn ""
--  parseSets s & \case
--     Left e -> putStrLn e
--     Right sets -> do
--       print sets 
--       putStrLn ""
--       let cards = sets ^.. rCardNames 
--       print cards 

--  putStrLn "" 
--  indexSets s & \case
--     Left e -> putStrLn e
--     Right mSets -> do
--       let vCards = M.lookup "LEA" mSets & fromJust 
--       let mCards = vCards & (V.fromList >>> fmap (\v -> (H.lookup v "name" & fromJust, v) ) >>> H.fromList) 
--       let m = mCards 
--       print $ M.lookup "ancestral recall" m 
--       readUntilEmpty $ \t -> do
--         let k = t & T.toCaseFold & T.words & T.unwords -- & T.splitOn " " & T.intercalate " " & T.toCaseFold
--         let v = M.lookup k m
--         print v 




 -- saveImagesFromMagicCardsInfo 1000 someMCICardIdentifiers  --- one second delay 

--  s <- getCards  
--  putStrLn ""
--  parseCards s & \case
--     Left e -> putStrLn e
--     Right j -> do
--       putStrLn ""
--       print j

--  putStrLn "" 
--  indexCards s & \case
--     Left e -> putStrLn e
--     Right m -> do
--       readUntilEmpty $ \t -> do
--         let k = t & T.toCaseFold & T.words & T.unwords -- & T.splitOn " " & T.intercalate " " & T.toCaseFold
--         let v = M.lookup k m
--         print v 

