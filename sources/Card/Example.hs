{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
{-# LANGUAGE LambdaCase, OverloadedStrings #-} 

module Card.Example where
import Card
import System.Environment
import Data.Function((&)) 
import qualified Data.Map as M 
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
-- import Prelude (head) 

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

someCardIdentifiers = fmap (defaultCardIdentifier "xln") [24..25 ] -- [1..21] -- [1..284] 
-- TODO some, like double-faced cards, don't have numerical collector numbers. for example, there is no 22, only 22a and 22b
-- get the real collectors numbers from the file  

mainWith _ = do
--  printCards 
 putStrLn "" 

 saveImagesFromMagicCardsInfo 1000 someCardIdentifiers  --- one second delay 

--  s <- getCards 
 
-- --  putStrLn "" 
-- --  parseCards s & \case
-- --     Left e -> putStrLn e
-- --     Right j -> do
-- --       print j

--  putStrLn "" 
--  indexCards s & \case
--     Left e -> putStrLn e
--     Right m -> do
--       readUntilEmpty $ \t -> do
--         let k = t & T.toCaseFold & T.words & T.unwords -- & T.splitOn " " & T.intercalate " " & T.toCaseFold
--         let v = M.lookup k m
--         print v 

