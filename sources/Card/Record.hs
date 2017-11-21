{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase, DeriveAnyClass, TypeOperators, OverloadedStrings #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications #-}

{-| 

-}
module Card.Record where 
-- import Card.Extra
import Prelude.Spiros hiding ((:*:)) 

import Data.Aeson.Types
import Data.Aeson

import Data.List.NonEmpty 
-- TODO import Data.Time
import Data.Functor.Identity
import GHC.TypeLits 
import Data.ByteString.Lazy (ByteString) 
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8 

-- type CardId = "id" :-> Text 
-- type CardObject = '[CardId] 

type (:::) = (:->) 
type Field s a = Identity (s ::: a)

type List a = [a]

field 
  :: forall (s :: Symbol) a. 
     a -> Field s a 
field = val 

z :: Record '[] 
z = RNil 

--------------------------------------------------------------------------------

-- exampleDecoding1 :: Either (J.ParseError String) CardRecord 
-- exampleDecoding1 = J.parse (fromJsonWithFormat (recordJsonFormat defaultJsonFormatRecord)) exampleCardObject  

-- exampleDecoding2 :: Either (J.ParseError String) LegalityRecord 
-- exampleDecoding2 = J.parse (fromJsonWithFormat (recordJsonFormat defaultJsonFormatRecord)) exampleLegalityObject  

--------------------------------------------------------------------------------

type CardRecord = Record Card 
type Card = 
         [ "name"          ::: Text 
        --  , "rulings"       ::: List (Record [ "language"     ::: Text 
        --                                     , "name"         ::: Text 
        --                                     , "multiverseid" ::: Maybe Natural 
        --                                     ]) 
         , "legalities"    ::: List (Record [ "format"   ::: Text 
                                            , "legality" ::: Text 
                                            ]) 
         ] 

type LegalityRecord = Record Legality
type Legality = 
  [ "format"   ::: Text 
  , "legality" ::: Text 
  ]

{-| 

@
{
        "artist": "Mark Poole",
        "cmc": 1,
        "colorIdentity": [
          "U"
        ],
        "colors": [
          "Blue"
        ],
        "id": "aa74b7dc3b30b2e7559598f983543755e226811d",
        "imageName": "ancestral recall",
        "layout": "normal",
        "legalities": [
          {
            "format": "Commander",
            "legality": "Banned"
          },
          {
            "format": "Legacy",
            "legality": "Banned"
          },
          {
            "format": "Vintage",
            "legality": "Restricted"
          }
        ],
        "manaCost": "{U}",
        "mciNumber": "48",
        "multiverseid": 95,
        "name": "Ancestral Recall",
        "originalText": "Draw 3 cards or force opponent to draw 3 cards.",
        "originalType": "Instant",
        "printings": [
          "LEA",
          "LEB",
          "2ED",
          "CED",
          "CEI",
          "VMA"
        ],
        "rarity": "Rare",
        "reserved": true,
        "text": "Target player draws three cards.",
        "type": "Instant",
        "types": [
          "Instant"
        ]
}

@ 

-}

exampleCardRecord :: CardRecord 
exampleCardRecord 
      = field @"name"
             "Ancestral Recall" 

    :&  field @"legalities" 
              [   ( field @"format"   "Commander"
                :&  field @"legality" "Banned"
                :&  z )  
              ,   ( field @"format"   "Legacy"
                :&  field @"legality" "Banned" 
                :&  z )
              ,   ( field @"format"   "Vintage"
                :&  field @"legality" "Restricted" 
                :&  z ) 
              ]
    :&  z 

{-| 

  {
    "name": "ancestral recall",
    "legalities": [
      {
        "format": "Commander",
        "legality": "Banned"
      },
      {
        "format": "Legacy",
        "legality": "Banned"
      },
      {
        "format": "Vintage",
        "legality": "Restricted"
      }
    ]
  } 


-}
exampleCardObject = 
    "\
    \{                                  \
    \  \"name\": \"ancestral recall\",  \
    \  \"legalities\": [                \
    \    {                              \
    \      \"format\": \"Commander\",   \
    \      \"legality\": \"Banned\"     \
    \    },                             \
    \    {                              \
    \      \"format\": \"Legacy\",      \
    \      \"legality\": \"Banned\"     \
    \    },                             \
    \    {                              \
    \      \"format\": \"Vintage\",     \
    \      \"legality\": \"Restricted\" \
    \    }                              \
    \  ]                                \
    \}                                  \
    \ "

-- exampleDecoding :: Either (J.ParseError String) CardRecord 
-- exampleDecoding = parseCardRecord exampleObject  
-- -- exampleDecoding = eitherDecode' exampleObject  

exampleLegalityObject = 
  "\
   \    {                              \
   \      \"format\":   \"Vintage\",   \
   \      \"legality\": \"Restricted\" \
   \    }                              \
   \"

  -- formatCardRecord :: JsonFormat String CardRecord
-- formatCardRecord = recordJsonFormat defaultJsonFormatRecord  

-- parserCardRecord :: J.Parse String CardRecord 
-- parserCardRecord = fromJsonWithFormat formatCardRecord 
--   -- recordFromJson formatCardRecord 

-- parseCardRecord :: ByteString -> Either (J.ParseError String) CardRecord 
-- parseCardRecord = J.parse parserCardRecord 

--------------------------------------------------------------------------------

formatLegalityRecord :: JsonFormat String LegalityRecord 
formatLegalityRecord  = recordJsonFormat defaultJsonFormatRecord 
