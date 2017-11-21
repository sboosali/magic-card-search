{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables  #-}

{-| 

the JSON schema for @AllCards-x.json@:

in my pseudocode, @Text+?@ and @?+Text@ both mean @Maybe [Text]@, i.e. an optional repetition of the type.
(note that the symbol order is reversed in those examples, but in either case, there applied outside-inwards)
@{ x :: Number, y :: Number}@ is a named record. 
@{ Bool | String }@ is an anonymous variant. 

@ 

{ name               :: Text
, code               :: Text
, gathererCode       :: Text?
, oldCode            :: Text?
, magicCardsInfoCode :: Text?
, releaseDate        :: Text?
, border             :: Text
, type               :: Text
, block              :: Text?
, onlineOnly         :: Bool?
, booster            :: ?+{ Text | Text+ }    
, cards              :: ?+{ id            :: Text 
                          , layout        :: Text 
                          , name          :: Text 
                          , names         :: Text+?
                          , manaCost      :: Text?
                          , cmc           :: Natural 
                          , colors        :: Text+?
                          , colorIdentity :: Text+?
                          , type          :: Text 
                          , supertypes    :: Text+?
                          , types         :: Text+?
                          , subtypes      :: Text+?
                          , rarity        :: Text 
                          , text          :: Text?
                          , flavor        :: Text?
                          , artist        :: Text
                          , number        :: Text?
                          , power         :: Text? 
                          , toughness     :: Text?  
                          , loyalty       :: Natural? 
                          , multiverseid  :: Natural?
                          , variations    :: Natural+? 
                          , imageName     :: Text? 
                          , watermark     :: Text? 
                          , border        :: Text? 
                          , timeshifted   :: Bool? 
                          , hand          :: Integer?  
                          , life          :: Integer? 
                          , reserved      :: Bool? 
                          , releaseDate   :: Text?
                          , starter       :: Bool?
                          , mciNumber     :: Text?  
                          , rulings       :: ?+{ language     :: Text 
                                               , name         :: Text 
                                               , multiverseid :: Natural?  
                                               } 
                          , foreignNames  :: ?+{ date :: Text 
                                               , text :: Text 
                                               } 
                          , printings     :: Text+ 
                          , originalText  :: Text? 
                          , originalType  :: Text?
                          , legalities    :: ?+{ format   :: Text 
                                               , legality :: Text 
                                               }
                          , source        :: Text? 
                          }
                        ]
} 

 
@ 

and the same schema, with named subrecords and using Haskell types for optionality and repetition: 

@

Set = 
  { name               :: Text
  , code               :: Text
  , gathererCode       :: (Maybe Text) 
  , oldCode            :: (Maybe Text) 
  , magicCardsInfoCode :: (Maybe Text) 
  , releaseDate        :: (Maybe Text) 
  , border             :: Text
  , type               :: Text
  , block              :: (Maybe Text) 
  , onlineOnly         :: (Maybe Bool)
  , booster            :: (Maybe Booster) 
  , cards              :: [Card]
  } 

Booster = 
  { Text 
  | [Text] 
  } 

Card = 
  { id            :: Text 
  , layout        :: Text 
  , name          :: Text 
  , names         :: Maybe [Text] 
  , manaCost      :: Maybe Text 
  , cmc           :: Natural 
  , colors        :: Maybe [Text] 
  , colorIdentity :: Maybe [Text] 
  , type          :: Text 
  , supertypes    :: Maybe [Text] 
  , types         :: Maybe [Text]
  , subtypes      :: Maybe [Text] 
  , rarity        :: Text 
  , text          :: Maybe Text 
  , flavor        :: Maybe Text 
  , artist        :: Text
  , number        :: Maybe Text
  , power         :: Maybe Text 
  , toughness     :: Maybe Text  
  , loyalty       :: Maybe Natural 
  , multiverseid  :: Maybe Natural
  , variations    :: Maybe [Natural] 
  , imageName     :: Maybe Text 
  , watermark     :: Maybe Text 
  , border        :: Maybe Text 
  , timeshifted   :: Maybe Bool 
  , hand          :: Maybe Integer  
  , life          :: Maybe Integer 
  , reserved      :: Maybe Bool 
  , releaseDate   :: Maybe Text
  , starter       :: Maybe Bool
  , mciNumber     :: Maybe Text  
  , rulings       :: Maybe [CardRulingObject] 
  , foreignNames  :: Maybe [CardForeignPrintingObject] 
  , printings     :: [Text]  
  , originalText  :: Maybe Text 
  , originalType  :: Maybe Text
  , legalities    :: Maybe [CardFormatLegalityObject]
  , source        :: Maybe Text 
  }

CardForeignPrintingObject = CardForeignPrintingObject 
  { language     :: Text 
  , name         :: Text 
  , multiverseid :: Maybe Natural 
  }

data CardRulingObject = 
  { date :: Text 
  , text :: Text 
  }

CardFormatLegalityObject = 
  { format   :: Text 
  , legality :: Text 
  } 

@

-}

module Card.Types where
import Card.Extra

import Prelude.Spiros() 
import Data.Aeson.Types (FromJSON(..),Options(..), SumEncoding(..), defaultOptions, genericParseJSON) 

import Data.List.NonEmpty 
-- TODO import Data.Time
-- import qualified Data.Text as T
import Data.Functor.Identity

{-| 

-}
data SetsObject = SetsObject (Map Text SetObject)
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,FromJSON) -- not Hashable 

{-| 

-}
data SetObject = SetObject 
  { _SetObject_name               :: Text  -- ^ "Nemesis",       // The name of the set
  , _SetObject_code               :: Text  -- ^ "NMS",           // The set's abbreviated code
  , _SetObject_gathererCode       :: Maybe Text  -- ^ "NE",            // The code that Gatherer uses for the set. Only present if different than 'code'
  , _SetObject_oldCode            :: Maybe Text  -- ^ "NEM",           // An old style code used by some Magic software. Only present if different than 'gathererCode' and 'code'
  , _SetObject_magicCardsInfoCode :: Maybe Text  -- ^ "ne",            // The code that magiccards.info uses for the set. Only present if magiccards.info has this set
  , _SetObject_releaseDate        :: Maybe Text  -- ^ "2000-02-14"     // When the set was released (YYYY-MM-DD). For promo sets, the date the first card was released.
  , _SetObject_border             :: Text  -- ^ "black",         // The type of border on the cards, either "white", "black" or "silver"
  , _SetObject_type               :: Text  -- ^ "expansion",     // Type of set. One of: "core", "expansion", "reprint", "box", "un", "from the vault", "premium deck", "duel deck", "starter", "commander", "planechase", "archenemy","promo", "vanguard", "masters", "conspiracy", "masterpiece"
  , _SetObject_block              :: Maybe Text  -- ^ "Masques",       // The block this set is in,
  , _SetObject_onlineOnly         :: Maybe Bool  -- ^ false,           // Present and set to true if the set was only released online
  , _SetObject_booster            :: Maybe MagicBoosterObject  -- ^ [ "rare", ... ], // Booster contents for this set, see below for details
  , _SetObject_cards              :: [CardObject]   -- ^ [ {}, {}, {}, ... ]  
  } deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)
instance FromJSON SetObject where parseJSON = parseJSON_TypePrefix

-- | see 'MagicBoosterSlotObject' 
type MagicBoosterObject = [MagicBoosterSlotObject] 

{-| 

"Each item in the array is either a string representing the type of booster card or an array of strings representing possible types for that booster card" 

@
[
      [
        "rare",
        "mythic rare"
      ],
      "uncommon",
      "uncommon",
      "uncommon",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
]
@

-}
data MagicBoosterSlotObject 
    = MagicBoosterSlotObjectSingle   Text 
    | MagicBoosterSlotObjectMultiple [Text] 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)
instance FromJSON MagicBoosterSlotObject  where parseJSON = genericParseJSON defaultOptions{ sumEncoding = UntaggedValue } 

{-| 

types suffixed with @Object@ (like this) represent schemas of the raw JSON Object. 

https://mtgjson.com/documentation.html

@
id
layout
name
names
manaCost
cmc
colors
colorIdentity
type
supertypes
types
subtypes
rarity
text
flavor
artist
number
power
toughness
loyalty
multiverseid
variations
imageName
watermark
border
timeshifted
hand
life
reserved
releaseDate
starter
mciNumber

-- extras-only fields
rulings
foreignNames
printings
originalText
originalType
legalities
source
@

the fields:

* @'_CardObject_id'@:            is 
* @'_CardObject_layout'@:        is 
* @'_CardObject_name'@:          is 
* @'_CardObject_names'@:         is 
* @'_CardObject_manaCost'@:      is 
* @'_CardObject_cmc'@:           is 
* @'_CardObject_colors'@:        is 
* @'_CardObject_colorIdentity'@: is 
* @'_CardObject_type'@:          is 
* @'_CardObject_supertypes'@:    is 
* @'_CardObject_types'@:         is 
* @'_CardObject_subtypes'@:      is 
* @'_CardObject_rarity'@:        is 
* @'_CardObject_text'@:          is 
* @'_CardObject_flavor'@:        is 
* @'_CardObject_artist'@:        is 
* @'_CardObject_number'@:        is 
* @'_CardObject_power'@:         is 
* @'_CardObject_toughness'@:     is 
* @'_CardObject_loyalty'@:       is 
* @'_CardObject_multiverseid'@:  is 
* @'_CardObject_variations'@:    is 
* @'_CardObject_imageName'@:     is 
* @'_CardObject_watermark'@:     is 
* @'_CardObject_border'@:        is 
* @'_CardObject_timeshifted'@:   is 
* @'_CardObject_hand'@:          is 
* @'_CardObject_life'@:          is 
* @'_CardObject_reserved'@:      is 
* @'_CardObject_releaseDate'@:   is 
* @'_CardObject_starter'@:       is 
* @'_CardObject_mciNumber'@:     is 
* @'_CardObject_rulings'@:       is 
* @'_CardObject_foreignNames'@:  is 
* @'_CardObject_printings'@:     is 
* @'_CardObject_originalText'@:  is 
* @'_CardObject_originalType'@:  is 
* @'_CardObject_legalities'@:    is 
* @'_CardObject_source'@:        is 

-}
data CardObject = CardObject 
  { _CardObject_id            :: Text 
  , _CardObject_layout        :: Text 
  , _CardObject_name          :: Text 
  , _CardObject_names         :: Maybe [Text] 
  , _CardObject_manaCost      :: Maybe Text 
  , _CardObject_cmc           :: Natural 
  , _CardObject_colors        :: Maybe [Text] 
  , _CardObject_colorIdentity :: Maybe [Text] 
  , _CardObject_type          :: Text 
  , _CardObject_supertypes    :: Maybe [Text] 
  , _CardObject_types         :: Maybe [Text] -- ^ Un-cards can have no type 
  , _CardObject_subtypes      :: Maybe [Text] 
  , _CardObject_rarity        :: Text 
  , _CardObject_text          :: Maybe Text 
  , _CardObject_flavor        :: Maybe Text 
  , _CardObject_artist        :: Text
  , _CardObject_number        :: Maybe Text
  , _CardObject_power         :: Maybe Text -- ^ Un-cards can have non-integer power/toughness 
  , _CardObject_toughness     :: Maybe Text  
  , _CardObject_loyalty       :: Maybe Natural 
  , _CardObject_multiverseid  :: Maybe Natural
  , _CardObject_variations    :: Maybe [Natural] 
  , _CardObject_imageName     :: Maybe Text 
  , _CardObject_watermark     :: Maybe Text 
  , _CardObject_border        :: Maybe Text 
  , _CardObject_timeshifted   :: Maybe Bool -- IsCardTimeShifted
  , _CardObject_hand          :: Maybe Integer  -- ^ Vanguard only 
  , _CardObject_life          :: Maybe Integer -- ^ Vanguard only 
  , _CardObject_reserved      :: Maybe Bool -- IsCardReserved 
  , _CardObject_releaseDate   :: Maybe Text -- ^ Promo only 
  , _CardObject_starter       :: Maybe Bool -- IsCardStarter 
  , _CardObject_mciNumber     :: Maybe Text  -- ^ used by `MagicCards.info`, almost always identical to '_CardObject_number' 
  , _CardObject_rulings       :: Maybe [CardRulingObject] 
  , _CardObject_foreignNames  :: Maybe [CardForeignPrintingObject] 
  , _CardObject_printings     :: [Text]  
  , _CardObject_originalText  :: Maybe Text 
  , _CardObject_originalType  :: Maybe Text
  , _CardObject_legalities    :: Maybe [CardFormatLegalityObject]
  , _CardObject_source        :: Maybe Text 
  }
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)
instance FromJSON CardObject where parseJSON = parseJSON_TypePrefix

{-| 

-}
data CardForeignPrintingObject = CardForeignPrintingObject 
  { _CardForeignPrintingObject_language     :: Text 
  , _CardForeignPrintingObject_name         :: Text 
  , _CardForeignPrintingObject_multiverseid :: Maybe Natural 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)
instance FromJSON CardForeignPrintingObject where parseJSON = parseJSON_TypePrefix

{-| 

-}
data CardRulingObject = CardRulingObject 
  { _CardRulingObject_date :: Text 
  , _CardRulingObject_text :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)
instance FromJSON CardRulingObject where parseJSON = parseJSON_TypePrefix
  
{-| 

-}
data CardFormatLegalityObject = CardFormatLegalityObject 
  { _CardFormatLegalityObject_format   :: Text 
  , _CardFormatLegalityObject_legality :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)
instance FromJSON CardFormatLegalityObject where parseJSON = parseJSON_TypePrefix


{- standard FromAeson instances: 

-- numbers 
FromJSON Int8
FromJSON Int1
FromJSON Int3
FromJSON Int6
FromJSON Int
FromJSON Word
FromJSON Word8
FromJSON Word16
FromJSON Word32
FromJSON Word64
FromJSON Number	 
FromJSON Natural
FromJSON Scientific	 

-- strings 
FromJSON Text	 
FromJSON Text	 

-- miscellaneous 
FromJSON Value		 
FromJSON ()	 
FromJSON Ordering
FromJSON Version

-- dates and times 
FromJSON LocalTime	 
FromJSON ZonedTime	
FromJSON TimeOfDay	 
FromJSON UTCTime	 
FromJSON NominalDiffTime	
FromJSON Day	 
FromJSON DiffTime	
FromJSON UUID	 
FromJSON DotNetTime	 	 
FromJSON CTime	 

-- collections 
FromJSON IntSet	  
FromJSON a => FromJSON [a]	 
FromJSON a => FromJSON (Maybe a)	 
(FromJSON a, Integral a) => FromJSON (Ratio a)	 
FromJSON a => FromJSON (Identity a)	 
FromJSON a => FromJSON (Min a)	 
FromJSON a => FromJSON (Max a)	 
FromJSON a => FromJSON (First a)	 
FromJSON a => FromJSON (Last a)	 
FromJSON a => FromJSON (WrappedMonoid a)	 
FromJSON a => FromJSON (Option a)	 
FromJSON a => FromJSON (NonEmpty a)	 
HasResolution a => FromJSON (Fixed a)	
FromJSON a => FromJSON (Dual a)	 
FromJSON a => FromJSON (First a)	 
FromJSON a => FromJSON (Last a)	 
FromJSON a => FromJSON (IntMap a)	 
FromJSON v => FromJSON (Tree v)	 
FromJSON a => FromJSON (Seq a)	 
(Ord a, FromJSON a) => FromJSON (Set a)	 
FromJSON a => FromJSON (DList a)	 
(Eq a, Hashable a, FromJSON a) => FromJSON (HashSet a)	 
FromJSON a => FromJSON (Vector a)	 
(Storable a, FromJSON a) => FromJSON (Vector a)	 
(Vector Vector a, FromJSON a) => FromJSON (Vector a)	 
(Prim a, FromJSON a) => FromJSON (Vector a)	 
(FromJSON a, FromJSON b) => FromJSON (Either a b)	 
(FromJSON a, FromJSON b) => FromJSON (a, b)	 
FromJSON (Proxy k a)	 
(FromJSONKey k, Ord k, FromJSON v) => FromJSON (Map k v)	 
(FromJSON v, FromJSONKey k, Eq k, Hashable k) => FromJSON (HashMap k v)	 

-}

------------------------------------------------------------------------------

{-| 

-}
data SetsData = SetsData (Map Text SetData)
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData) -- not Hashable 

{-| 

-}
data SetData = SetData 
  { _SetData_name               :: Text  -- ^ "Nemesis",       // The name of the set
  , _SetData_code               :: Text  -- ^ "NMS",           // The set's abbreviated code
  , _SetData_gathererCode       :: Maybe Text  -- ^ "NE",            // The code that Gatherer uses for the set. Only present if different than 'code'
  , _SetData_oldCode            :: Maybe Text  -- ^ "NEM",           // An old style code used by some Magic software. Only present if different than 'gathererCode' and 'code'
  , _SetData_magicCardsInfoCode :: Maybe Text  -- ^ "ne",            // The code that magiccards.info uses for the set. Only present if magiccards.info has this set
  , _SetData_releaseDate        :: Maybe Text  -- ^ "2000-02-14"     // When the set was released (YYYY-MM-DD). For promo sets, the date the first card was released.
  , _SetData_border             :: Text  -- ^ "black",         // The type of border on the cards, either "white", "black" or "silver"
  , _SetData_type               :: Text  -- ^ "expansion",     // Type of set. One of: "core", "expansion", "reprint", "box", "un", "from the vault", "premium deck", "duel deck", "starter", "commander", "planechase", "archenemy","promo", "vanguard", "masters", "conspiracy", "masterpiece"
  , _SetData_block              :: Maybe Text  -- ^ "Masques",       // The block this set is in,
  , _SetData_onlineOnly         :: Maybe Bool  -- ^ false,           // Present and set to true if the set was only released online
  , _SetData_booster            :: Maybe MagicBoosterData  -- ^ [ "rare", ... ], // Booster contents for this set, see below for details
  , _SetData_cards              :: [CardData]   -- ^ [ {}, {}, {}, ... ]  
  } deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data SetDataF f = SetDataF  
  { _SetDataF_name               :: f Text  -- ^ 
  , _SetDataF_code               :: f Text  -- ^ 
  , _SetDataF_gathererCode       :: f (Maybe Text)  -- ^ 
  , _SetDataF_oldCode            :: f (Maybe Text)  -- ^ 
  , _SetDataF_magicCardsInfoCode :: f (Maybe Text)  -- ^ 
  , _SetDataF_releaseDate        :: f (Maybe Text)  -- ^ 
  , _SetDataF_border             :: f Text  -- ^ 
  , _SetDataF_type               :: f Text  -- ^ 
  , _SetDataF_block              :: f (Maybe Text)  -- ^ 
  , _SetDataF_onlineOnly         :: f (Maybe Bool)  -- ^ 
  , _SetDataF_booster            :: f (Maybe MagicBoosterData) -- ^ 
  , _SetDataF_cards              :: f [CardData]   -- ^ 
  } 
-- deriving (Functor) -- TODO it's a higher order, HFunctor
-- deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)
-- deriving instance Functor SetDataF 

type SetDataI = SetDataF Identity 

type MagicBoosterData = NonEmpty (NonEmpty MagicBoosterSlotData)

type MagicBoosterSlotData = Text 

{-| a more richly-typed 'CardObject'. i.e. parsed, simplified, and validated.

documentation for the fields is below.  

* @'_CardData_id'@: 

gameplay-relevant stuff, card characteristics: 

* @'_CardData_name'@: 
* @'_CardData_manaCost'@: 
* @'_CardData_colors'@: 
* @'_CardData_type'@: 
* @'_CardData_text'@: 

quasi-derivable stuff, card characteristics: 

* @'_CardData_cmc'@: 
* @'_CardData_colorIdentity'@: 
* @'_CardData_names'@: 
* @'_CardData_type'@: 

not actually derivable, without some special casing, because color markers. e.g. Pact Of Negation has a blue dot on the left of the type line (with accompanying reminder text for people with visual impairments), which makes it blue as a characteristic. 

non-gameplay-relevant stuff, card characteristics: 

* @'_CardData_layout'@: 
* @'_CardData_watermark'@: 
* @'_CardData_rarity'@: 
* @'_CardData_flavor'@: 
* @'_CardData_artist'@: 
* @'_CardData_number'@: 

Internet data: 

* @'_CardData_multiverseid'@: 
* @'_CardData_mciNumber'@: 

links the card to other resources, like images or results from from the two standard magic card search engines (below): 

data for other printings: 
 
* @'_CardData_variations'@: 
* @'_CardData_printings'@: 
* @'_CardData_originalText'@: 
* @'_CardData_originalType'@: 
* @'_CardData_foreignNames'@: 

miscellaneous data:  

* @'_CardData_rulings'@: 
* @'_CardData_legalities'@: 

-}
data CardData = CardData 
  { _CardData_id            :: CardId 

  -- gameplay-relevant stuff, card characteristic 
  , _CardData_name          :: CardName 
  , _CardData_manaCost      :: Maybe ManaCost 
  , _CardData_colors        :: [CardColor] 
  , _CardData_type          :: CardTypeLine 
  , _CardData_text          :: CardText 

  -- quasi-derivable stuff, card characteristics 
  , _CardData_cmc           :: ConvertedManaCost 
  , _CardData_colorIdentity :: [CardColorIdentity] 
  , _CardData_names         :: Maybe [CardName] 
  , _CardData_supertypes    :: [CardSupertype] 
  , _CardData_types         :: (NonEmpty CardTypes) 
  , _CardData_subtypes      :: [CardSubtype] 

  -- non-gameplay-relevant stuff, card characteristics 
  , _CardData_layout        :: CardLayout 
  , _CardData_watermark     :: Maybe Text 
  , _CardData_rarity        :: KnownCardRarity 
  , _CardData_flavor        :: CardFlavorText 
  , _CardData_artist        :: CardArtist
  , _CardData_number        :: CardCollectorNumber 

  -- other resources
  , _CardData_multiverseid  :: WizardsIdentifier 
  , _CardData_mciNumber     :: CardCollectorNumber -- ^ used by `MagicCards.info`, almost always identical to '_CardData_number'

  , _CardData_rulings       :: [CardRuling] 
  , _CardData_legalities    :: [CardFormatLegality]

  , _CardData_variations    :: [WizardsIdentifier] 
  , _CardData_printings     :: [CardSetCode] 
  , _CardData_originalText  :: Maybe CardText 
  , _CardData_originalType  :: Maybe CardTypeLine 
  , _CardData_foreignNames  :: [CardForeignPrinting] 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-

-- stuff I don't care about, though the Vanguard-only characteristics below can be moved up to the type-dependent-characteristics above 
, _CardData_starter       :: Bool -- IsCardStarter
, _CardData_source        :: Maybe Text
, _CardData_imageName     :: Maybe Text
, _CardData_releaseDate   :: Maybe CardReleaseDate -- ^ Promo only
, _CardData_life          :: Maybe VanguardLifeModifier -- ^ Vanguard only
, _CardData_hand          :: Maybe VanguardHandModifier -- ^ Vanguard only

-- set specific stuff: old sets, time spiral, and UN-sets 
, _CardData_reserved      :: Bool -- IsCardReserved
, _CardData_timeshifted   :: Bool -- IsCardTimeShifted
, _CardData_border        :: Maybe CardBorderColor

-}

{-| the number on the right of some cards. 

nothing has either power or toughness without having both. 

nothing with a power/toughness has a loyalty.

-}
data CardCharacteristicNumber 
  = CardCharacteristicPowerToughness CardNumber CardNumber 
  | CardCharacteristicLoyalty CardNumber 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| for power, toughness, loyalty. 

-}
data CardNumber 
  = CardIntegerNumber  Integer -- ^ the printed number, the most frequent case. can be negative: e.g. Char-Rumbler, which has a power of @'CardIntegerNumber' -1@. (Un-cards can have non-integer power/toughness, which we're ignoring)
  | CardWildcardNumber Integer -- ^ the integer represents the modifier: @1@ is @\*+1@, @0@ is just @\*@. e.g. Tarmogoyf has a power of  @'CardWildcardNumber' 0@ and a toughness of @'CardWildcardNumber' 1@. 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable) 

{-| unifies the following, with invariants:  

* '_CardObject_supertypes'
* '_CardObject_types'
* '_CardObject_subtypes'

-}
data CardTypes = CardTypes 
  { _CardTypes_supertypes :: [CardSupertype] 
  , _CardTypes_types      :: NonEmpty CardType 
  , _CardTypes_subtypes   :: [CardSubtype] 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| most non-creature cards have a single card type, without supertypes or subtypes. 

-}
defaultCardTypes :: CardType -> CardTypes
defaultCardTypes t = CardTypes{..} 
  where 
  _CardTypes_supertypes = []
  _CardTypes_types      = t :| [] 
  _CardTypes_subtypes   = [] 

-- TODO pair the subtype to the card type , except with the tribal supertype , which provides creature subtypes to non-creature cartoons 
{-| 

-}
data CardSupertype
  = KnownCardSupertype KnownCardSupertype 
  | UnknownCardSupertype Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardType
 = KnownCardType KnownCardType 
 | UnknownCardType Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardSubtype 
  = KnownCardSubtype KnownCardSubtype 
  | UnknownCardSubtype Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)


{-| 

-}
newtype ConvertedManaCost = ConvertedManaCost Natural 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| the unique card identifier. 

for each set in @AllSets.json@, every card has an identifier that distinct from every other card, including the same cardname in other sets.

-}
data CardId = CardId Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardName = CardName Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data ManaCost = ManaCost Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-|  

-}
data CardColor = CardColor Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardTypeLine = CardTypeLine Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardText = CardText Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable) 

{-| 

-}
data CardColorIdentity = CardColorIdentity Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardWholeName = CardWholeName Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardLayout = CardLayout Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardWatermark = CardWatermark Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data KnownCardRarity  
  = Common 
  | Uncommon 
  | Rare 
  | Mythic 
  | Timeshifted -- TODO
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)
  
{-| 

-}
newtype CardFlavorText = CardFlavorText Text 
 deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable) 

{-| 

-}
newtype CardArtist = CardArtist Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardCollectorNumber = CardCollectorNumber Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| the Multiverse ID, used by `gather.wizards.com`. 

-}
data WizardsIdentifier = WizardsIdentifier Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| used by `MagicCards.info`, almost always identical to '_CardData_number' 

-}
data MagicCardsInfoIdentifier = MagicCardsInfoIdentifier Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardSetCode = CardSetCode Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardForeignPrinting = CardForeignPrinting 
  { _CardForeignPrinting_language     :: KnownLanguage 
  , _CardForeignPrinting_name         :: CardName 
  , _CardForeignPrinting_multiverseid :: WizardsIdentifier 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardRuling = CardRuling 
  { _CardRuling_date :: Text -- TODO Day needs a hashable instance 
  , _CardRuling_text :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardFormatLegality = CardFormatLegality 
  { _CardFormatLegality_format   :: KnownMagicFormat 
  , _CardFormatLegality_legality :: KnownMagicLegality 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data KnownMagicFormat = KnownMagicFormat Text -- TODO 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data KnownMagicLegality 
  = Legal 
  | Restricted 
  | Banned 
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)

{-| 

-}
data KnownLanguage
    = LanguageEN
    | LanguageES 
    deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)

data KnownCardSupertype 
    = TribalSupertype 
    | SnowSupertype 
    deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)

data KnownCardType 
    = InstantType 
    | SorceryType 
    deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)

type KnownCardSubtype = Text -- TODO 
