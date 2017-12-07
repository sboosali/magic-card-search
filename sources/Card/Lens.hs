{-# LANGUAGE TemplateHaskell #-}

module Card.Lens where
import Card.Extra (concatenateA) 
import Card.Schema 
import Control.Lens (makeLenses) 

concatenateA makeLenses
  [ ''SetsObject 
  , ''SetObject 
  , ''CardObject
  , ''MagicBoosterSlotObject 
  , ''CardForeignPrintingObject
  , ''CardRulingObject
  , ''CardFormatLegalityObject
  ]
