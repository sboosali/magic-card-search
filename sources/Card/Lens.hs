{-# LANGUAGE TemplateHaskell #-}

module Card.Lens where
import Card.Extra (concatenateA) 
import Card.Schema 
import Control.Lens (makeLenses,makePrisms) 

concatenateA makeLenses
  [ ''SetObject 
  , ''CardObject
  , ''CardForeignPrintingObject
  , ''CardRulingObject
  , ''CardFormatLegalityObject
  ]

concatenateA makePrisms
  [ ''SetsObject 
  , ''MagicBoosterSlotObject 
  ]  

{-


e.g. look up a card by name 

import qualified Data.List as List 
let cs = o ^.. _SetsObject . ix "LEA" . setObject_cards & concat
let c = cs & List.find (\c -> c^.cardObject_name == "Elvish Archers") & fromJust 
let vanilla = cs & List.find (\c -> c^.cardObject_name == "Grizzly Bears") & fromJust

CardObject { _CardObject_name = "Elvish Archers", _CardObject_types = Just ["Creature"], _CardObject_subtypes = Just ["Elf","Archer"], ... }

>>> maybeToList ((,) <$> (c^.cardObject_types) <*> (c^.cardObject_subtypes)) >>= (\(ts,ss) -> (,) <$> ts <*> ss)

[("Creature","Elf"),("Creature","Archer")]

>>> (,) <$> (c^. cardObject_types._Just) <*> (c ^. cardObject_subtypes._Just)

[("Creature","Elf"),("Creature","Archer")]



e.g. getting all unique printed subtypes in Alpha

>>> o ^.. _SetsObject . ix "LEA" . setObject_cards . traverse . cardObject_subtypes & (fmap (maybeToList >>>  concat) >>> concat >>> sort >>> nub) 

["Beast","Juggernaut","Wall","Golem","Aura","Human","Knight","Wraith","Demon","Skeleton","Shade","Specter","Spirit","Imp","Nightmare","Horse","Rat","Assassin","Zombie","Vampire","Elemental","Shapeshifter","Merfolk","Djinn","Illusion","Pirate","Wizard","Serpent","Bird","Cockatrice","Wurm","Elf","Archer","Fungus","Dinosaur","Avatar","Spider","Bear","Treefolk","Druid","Faerie","Nymph","Dryad","Basilisk","Wolf","Plant","Elephant","Dragon","Dwarf","Warrior","Goblin","Gargoyle","Ogre","Giant","Minotaur","Orc","Barbarian","Hydra","Troll","Soldier","Pegasus","Unicorn","Incarnation","Cleric","Cat","Angel","Swamp","Mountain","Forest","Plains","Island"]


e.g. Alpha subtypes, grouped by supertype (almost mutually exclusive)

import qualified Data.Map as Map
import qualified Data.Set as Set
let cs = o ^.. _SetsObject . ix "LEA" . setObject_cards & concat

cs & mapMaybe (\c -> ((,) <$> (c^.cardObject_types >>= listToMaybe) <*> c^.cardObject_subtypes)) & (groupBy ((==) `on` (view _2)) >>> concat) & (Map.fromListWith (<>) >>> Map.map Set.fromList)

fromList [("Artifact",fromList ["Beast","Golem","Juggernaut","Wall"]),("Creature",fromList ["Angel","Archer","Assassin","Avatar","Barbarian","Basilisk","Bear","Bird","Cat","Cleric","Cockatrice","Demon","Dinosaur","Djinn","Dragon","Druid","Dryad","Dwarf","Elemental","Elephant","Elf","Faerie","Fungus","Gargoyle","Giant","Goblin","Horse","Human","Hydra","Illusion","Imp","Incarnation","Knight","Merfolk","Minotaur","Nightmare","Nymph","Ogre","Orc","Pegasus","Pirate","Plant","Rat","Serpent","Shade","Shapeshifter","Skeleton","Soldier","Specter","Spider","Spirit","Treefolk","Troll","Unicorn","Vampire","Wall","Warrior","Wizard","Wolf","Wraith","Wurm","Zombie"]),("Enchantment",fromList ["Aura"]),("Land",fromList ["Forest","Island","Mountain","Plains","Swamp"])]


cs & concatMap (\c -> (,) <$> (c^. cardObject_types._Just) <*> (c ^. cardObject_subtypes._Just)) & over (mapped._2) Set.singleton & (Map.fromListWith (Set.union))

fromList [("Artifact",fromList ["Beast","Golem","Juggernaut","Wall"]),("Creature",fromList ["Angel","Archer","Assassin","Avatar","Barbarian","Basilisk","Bear","Beast","Bird","Cat","Cleric","Cockatrice","Demon","Dinosaur","Djinn","Dragon","Druid","Dryad","Dwarf","Elemental","Elephant","Elf","Faerie","Fungus","Gargoyle","Giant","Goblin","Golem","Horse","Human","Hydra","Illusion","Imp","Incarnation","Juggernaut","Knight","Merfolk","Minotaur","Nightmare","Nymph","Ogre","Orc","Pegasus","Pirate","Plant","Rat","Serpent","Shade","Shapeshifter","Skeleton","Soldier","Specter","Spider","Spirit","Treefolk","Troll","Unicorn","Vampire","Wall","Warrior","Wizard","Wolf","Wraith","Wurm","Zombie"]),("Enchantment",fromList ["Aura"]),("Land",fromList ["Forest","Island","Mountain","Plains","Swamp"])]


e.g.all cards

let cs = o ^.. _SetsObject . traverse . setObject_cards . traverse

> length cs
34818

import Control.Arrow
import Data.Monoid
import Data.Foldable
import qualified Data.Text.Lazy.IO as TL
let ns = cs & fmap (view cardObject_name &&& id) & Map.fromListWith const

Map.lookup "Ancestral Recall" ns

ts = cs & filter (\CardObject{..} -> (_CardObject_subtypes ^. _Just) & fmap (\t -> Any $ t `TL.isInfixOf` (_CardObject_text ^. _Just)) & fold & getAny)
-- but, it counts names too, like Serra Angel 

ts = cs & filter (\CardObject{..} -> let text = (_CardObject_text ^. _Just) & TL.replace _CardObject_name "~" in (_CardObject_subtypes ^. _Just) & fmap (\subtype -> Any $ subtype `TL.isInfixOf` text) & fold & getAny) & fmap (_CardObject_name &&& id) 
-- include spells, like Dispel 

ts = cs & filter (\CardObject{..} -> let text = (_CardObject_text ^. _Just) & TL.replace _CardObject_name "~" in (_CardObject_subtypes ^. _Just) & fmap (\subtype -> Any $ subtype `TL.isInfixOf` text) & fold & getAny) & filter (\CardObject{..} -> "Creature" `elem` _CardObject_types) & fmap (_CardObject_name &&& id) 

ts = cs & filter (\CardObject{..} -> let text = (_CardObject_text ^. _Just) & TL.replace _CardObject_name "~" in (_CardObject_subtypes ^. _Just) & fmap (\subtype -> Any $ subtype `TL.isInfixOf` text) & fold & getAny) & filter (\CardObject{..} ->  _CardObject_types & anyOf (_Just.traverse) (=="Creature")) & fmap (_CardObject_name &&& id)

ts & fmap snd & mapMaybe (\CardObject{..} -> (,,) <$> Just _CardObject_name <*> _CardObject_subtypes <*> _CardObject_text) & traverse_ (\(n,ts,x) -> TL.putStrLn n >> TL.putStrLn (TL.intercalate ", " ts) >> TL.putStrLn x >> TL.putStrLn "")

lords = ts & fmap snd & mapMaybe (\CardObject{..} -> (,,) <$> Just _CardObject_name <*> _CardObject_subtypes <*> _CardObject_text) & fmap (\(n,ts,x) -> [n, ts & TL.intercalate ", ", x, "\n"] & TL.intercalate "\n") & TL.intercalate "\n"

>>> TL.putStrLn $ lords & TL.take 100
Graveborn Muse
Zombie, Spirit
At the beginning of your upkeep, you draw X cards and you lose X life, where X is the number of Zombies you control.

>>> TL.writeFile "lords.txt" lords

-}