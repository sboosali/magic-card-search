{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- 
{-# LANGUAGE LambdaCase, OverloadedStrings #-} 

module Card.Example where
import Card

import qualified Card.JSONSchema

import System.Environment
import Data.Function((&)) 
-- import qualified Data.Map as M 
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS 
-- import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8 
-- import Prelude (head) 
import Control.Lens hiding ((<&>)) 
import Data.Aeson
import Data.Aeson.Lens
-- import qualified Data.HashMap.Strict as H 
-- import Data.HashMap.Strict  (HashMap) 
import Data.Maybe 
-- import qualified Data.Vector as V 
import Control.Arrow((>>>)) 
import Prelude.Spiros

----------------------------------------

mainWith _ = do
 putStrLn ""
 Card.JSONSchema.main

----------------------------------------

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

someMCICardIdentifiers = map (defaultMCICardIdentifier "xln") [24..284 ] -- [1..21] -- [1..284] 
-- TODO some, like double-faced cards, don't have numerical collector numbers. for example, there is no 22, only 22a and 22b
-- get the real collectors numbers from the file  

--mainWith _ = do
-- putStrLn ""
-- Card.JSONSchema.main
 
 --TODO mainGetCards >>= print

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
---------------------------------------

printAllSets = getSetsDefault >>= B8.putStrLn

printAllCards = getCardsDefault >>= B8.putStrLn

mainGetCards = do
 _o <- readSetsObjectDefault
 s <- getSetsDefault -- getOneSet
 putStrLn ""
-- print$ s ^.. members . key "cards" . _Array . traverse . key "text" . _String
 putStrLn ""
 -- print$ s ^.. members . key "cards" . _Array . traverse . (key "name" <> key "text") . _String -- 2 Getters appends into a Fold
-- print$ s ^.. members . key "cards" . _Array . traverse . (runFold ((,) <$> Fold (key_Object "name") <*> Fold (key_Object "text"))) . both . _String -- Reified optics; Getter nor Traversal didn't work
 let cs = asCardNameTextPairs s
 print cs

 readUntilEmpty $ \t -> do
   let ds = findCardsByText cs t
   let es = ds & fmap T.unpack & zip [1::Int ..]
   let pp = traverse_ (\(i,c) -> putStr (show i) >> putStr " " >> putStrLn c)
   pp es
   putStrLn""

findCardsByText cs t = cs & mapMaybe (doesCardHaveText q) 
  where
  q = t & normalize 
  normalize = T.toCaseFold >>> T.words >>> T.unwords  

doesCardHaveText q _c@(n,t) = if T.isInfixOf q t then Just n else Nothing 
  
-- findCardByText m t = v
--   where
--   k = t & T.toCaseFold & T.words & T.unwords 
--   v = M.lookup k m

asCardNameTextPairs s = ds
  where
  cs = s ^.. members . key "cards" . _Array . traverse . (runFold ((,) <$> Fold (key_Object "name" . _String) <*> Fold (key_Object "text" . _String)))
  ds = cs & each . both %~ T.toCaseFold

key_Object :: TS.Text -> Traversal' Value Value
key_Object = key

{-

let ds = (cs ^.. traverse . traverse . (runFold ((,,) <$> Fold (key_Object "name" . _String) <*> Fold (key_Object "id" . _String) <*> Fold (key_Object "subtypes" . _Array)))) 
 ds & each . _3 . each %~ (preview  _String) 
[("Scathe Zombies","571e2f42fa8092562bc1108e87330641eb054ab1",[Just "Zombie"])]
it :: [(TS.Text, TS.Text, Data.Vector.Vector (Maybe TS.Text))]

es = ds & each . _3 . each %~ preview  _String



-}

-- ("hello","world","!!!")^.runGetter ((,) <$> Getter _2 <*> Getter (_1.to(length)))

--  parseSets s & \case
--     Left e -> putStrLn e
--     Right sets' -> do
--       -- print sets 
-- --      putStrLn ""
--       let cards' = sets' ^.. members -- values
-- --      putStrLn ""
--       let cards = cards' & mapped . key "text" . _String %~ T.toCaseFold
--       print cards'
--       let names = cards ^.. each . key "text" . _String
--       let texts = cards ^.. each . key "text" . _String
--       print $ zip names texts 
--       putStrLn ""
-- --       readUntilEmpty $ \t -> do
-- --         let k = t & T.toCaseFold & T.words & T.unwords 
-- --         let v = M.lookup k m


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

