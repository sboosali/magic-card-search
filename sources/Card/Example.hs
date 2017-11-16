{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Card.Example where
import Card
import System.Environment

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

mainWith s = do
 putStrLn s
 printCards 
 -- getCards parseCards 
