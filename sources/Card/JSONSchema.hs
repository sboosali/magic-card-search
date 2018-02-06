{-# LANGUAGE DataKinds, OverloadedLists, OverloadedStrings, TypeApplications #-}
{-# LANGUAGE TypeOperators, PolyKinds #-}

{-# LANGUAGE UndecidableInstances #-} -- for TypeError

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-} 

{-|


see:

@
$ ./munge-AllSets-x.sh
@


@
$ jq -c '{ LEA, ARN, ATQ, LEG, DRK, FEM, ICE, CHR, HML, ALL, MIR, VIS, POR, WTH, TMP, STH, PO2, EXO, USG, ULG, UDS, PTK, MMQ, NMS, PCY, INV, PLS, APC, ODY, TOR, JUD, ONS, LGN, SCG, MRD, DST, ("5DN"): .["5DN"], CHK, BOK, SOK, RAV, GPT, DIS, CSP, TSP, PLC, FUT, LRW, MOR, SHM, EVE, ALA, CON, ARB, M10, HOP, ZEN, WWK, ROE, M11, SOM, MBS, NPH, CMD, M12, ISD, DKA, PC2, AVR, M13, RTR, GTC, DDK, DGM, MMA, M14, THS, C13, BNG, JOU, CNS, M15, KTK, C14, FRF, DTK, ORI, BFZ, C15, OGW, W16, SOI, EMA, EMN, CN2, KLD, C16, AER, AKH, HOU, C17, XLN, RIX }' AllSets-x.json > RealSets-x.json

-- NOTE `("5DN"): .["5DN"]` has a numeric prefix, and thus can't be an identifier.
- 105 sets

$ jq '.[] |= {code, name, cards: .cards | map({ name,id,manaCost,cmc,colors,colorIdentity,type,rarity,text,flavor,power,toughness,loyalty,artist,mciNumber,rulings,legalities })}' RealSets-x.json > RealSets-y.json


$ jq '{RIX,XLN}' RealSets-y.json > RIXSets-y.json
@


-}
module Card.JSONSchema where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString) 

import Data.Vinyl
import Data.Schematic

--import qualified Data.Text as T
--import Data.Text (Text)

import GHC.TypeLits hiding (Text)
import qualified GHC.TypeLits as GHC

import Paths_magic_card_search

---------------------------------------

{-NOTES

data Schema
  = SchemaText [TextConstraint]
  | SchemaBoolean
  | SchemaNumber [NumberConstraint]
  | SchemaObject [(Symbol, Schema)]
  | SchemaArray [ArrayConstraint] Schema
  | SchemaNull
  | SchemaOptional Schema
  | SchemaUnion [Schema]

data TextConstraint
  = TEq Nat
  | TLt Nat
  | TLe Nat
  | TGt Nat
  | TGe Nat
  | TRegex Symbol
  | TEnum [Symbol]

-}

----------------------------------------

-- | type-level pair.
type (:-) a b = '(a,b)
infixr 1 :-

-- | type-level list append.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- | type-level list 'zip'. 
type family ZIP xs ys where
  ZIP '[]       '[]       = '[]
  ZIP (x ': xs) (y ': ys) = '(x,y) ': ZIP xs ys
  ZIP _         _         = TypeError (GHC.Text "[ZIP xs ys] `xs` and `ys` must have the same length)")

-- | type-level list 'replicate'. 
type family REPLICATE n x where
  REPLICATE 0 x = '[]
  REPLICATE n x = x ': (REPLICATE (n-1) x)

-- | type-level list 'length'. 
type family LENGTH xs where
  LENGTH '[]       = 0
  LENGTH (x ': xs) = 1 + LENGTH xs

{-| all keys share the same value type.

e.g.

@
> :kind! HomogeneousObject ["x","y","z"] Int
[ '("x",Int), '("y",Int), '("z",Int) ]   
@

-}
type HomogeneousObject ks v = ZIP ks (REPLICATE (LENGTH ks) v)

------------------------------------------
  
--type TEnum es = ()

{-| unconstrained json array. 
-}
type ArraySchema a = SchemaArray '[] a

{-| 
-}
type EitherSchema a b = SchemaUnion '[a,b]

--type LengthSchema a = ()

{-|

@AlphaNumericSchema@ represents an alphanumeric string. 

-}
type AlphaNumericSchema =
  SchemaText '[ TRegex "[0-9a-fA-Z]*" ] --TODO

{-|

@HexSchema n@ represents a @n@-digit hexadecimal number. 

-}
type HexSchema n =
  SchemaText '[ TEq n, TRegex "[0-9a-f]+" ]
-- type HexSchema n = SchemaText '[ TRegex "[0-9a-f]{" (TShow n) "}" ]

{-| an (otherwise unconstrained) json number. 
-}
type NUMBER = SchemaNumber '[]

{-| unconstrained json text
-}
type TEXT = SchemaText '[]

{-| json text, constrained to match the given regex.
-}
type REGEX s = SchemaText '[TRegex s]

{-| json text, constrained to be one of the given strings.
-}  
type ENUM ss = SchemaText '[TEnum ss]
--type ENUM ss = TextConstraint (TEnum ss)

{-| see 'REGEX'
-}  
type REGEX_IF strictness s =
  TEXT_UNLESS strictness (REGEX s)

{-| see 'ENUM'
-}  
type ENUM_IF strictness ss =
   TEXT_UNLESS strictness (ENUM ss) 


{-| see 'TEXT'
-}  
type TEXT_UNLESS strictness schema =
  UNLESS_STRICT strictness TEXT
    schema
 
------------------------------------------
  
{-| type-level conditional. 
-}  
type family IF b t e where
  IF 'True  t e = t
  IF 'False t e = e

{-| type-level conditional.

a flipped 'IF'.

-}  
type UNLESS b e t = IF b t e 

{-| @data kind@, like @Bool@.
-}
data Strictness = Strict | Loose

{-| like 'IF', but with a custom boolean kind, 'Strictness'.
-}
type IF_STRICT strictness strict loose =
  IF (StrictnessToBoolean strictness) strict loose
  -- IF_STRICT 'Strict strict loose = strict
  -- IF_STRICT 'Loose  strict loose = loose

{-| like 'UNLESS', but with a custom boolean kind, 'Strictness'.
-}
type UNLESS_STRICT strictness loose strict =
  IF_STRICT strictness strict loose

{-| whether the @strictness@ is 'Strict'. -}
type family StrictnessToBoolean strictness where
  StrictnessToBoolean 'Strict = 'True
  StrictnessToBoolean 'Loose  = 'False

---------------------------------------
-- SCHEMAS
---------------------------------------

-- | 
type Strict_SetsSchema = SetSchema 'Strict

{-| 

<https://mtgjson.com/documentation.html>

@AllSets-x.json@ schema:

@

@

the @strictness@ conditional because: new sets are constantly introduced. 

-}
type SetsSchema strictness = SchemaArray '[] (SetSchema strictness)

{-

type SetsSchema strictness = SchemaObject
 (IF_STRICT strictness
  (HomogeneousObject SetNamesList (SetSchema strictness))
  '[]
 )


-- ERROR:
--         1
--         + LENGTH
--             '["LEG", "UGL", "DIS", "ZEN", "MMQ", "W17", "LRW", "DDI", "pELP",
--               "C13", "pWPN", "SOM", "DD3_GVL", "PC2", "V16", "USG", "CHR",
--               "pMEI", "MMA"]
--       Use -freduction-depth=0 to disable this check
--       (any upper bound you could choose might fail unpredictably with
--        minor updates to GHC, so disabling the check is recommended if
--        you're sure that type checking should terminate)


{-| 

-}
type SetNamesList =   
 '[ "V13","ALA","DDL","5ED","C16","CST","MBS","CMA","ICE","E02","OGW","PD3","CEI","pPRE","DDG","M13","CM1","HOU","CON","DKM","V15","3ED","AKH","UST","MM2","PCA","CED","XLN","DDJ","pPRO","DDM","ITP","C17","ARN","pSUS","p15A","TSB","EXP","RAV","SOI","ATH","V12","LGN","pLPA","IMA","pALP","DRK","pWCQ","4ED","p2HG","LEA","CP3","MGB","9ED","M14","DDP","GTC","ARC","CHK","pGRU","ROE","DD3_EVG","V14","WTH","DTK","MM3","2ED","MRD","DDK","RIX","S99","BNG","DD3_JVC","DST","7ED","V11","pLGM","ULG","BOK","CNS","AER","MIR","DDN","C14","MPS","BFZ","EMN","DDQ","TOR","EMA","ARB","TMP","INV","HML","H09","ALL","8ED","CP2","VAN","M15","RTR","DDD","M10","ORI","DDT","KTK","EVG","ME4","E01","10E","ATQ","DRB","VIS","MED","pGTW","PTK","pGPX","DDO","SCG","STH","C15","HOP","6ED","CN2","5DN","PLS","pHHO","V09","THS","pARL","SOK","PLC","V10","TSP","pMPR","FUT","ONS","UDS","EXO","pCEL","PCY","BRB","CP1","BTD","FRF","ODY","CMD","DDR","DD3_DVD","DD2","APC","VMA","ME2","pWOR","pJGP","pDRC","POR","PO2","pFNM","DDE","M11","AVR","DPA","NMS","CSP","MOR","V17","RQS","W16","DDH","FRF_UGIN","MD1","JOU","WWK","DDC","GPT","pSUM","DDS","pREL","UNH","JUD","pPOD","ME3","pWOS","MPS_AKH","LEB","KLD","S00","NPH","ISD","DDF","M12","pMGD","pCMP","FEM","DGM","TPR","DKA","EVE","SHM","PD2","LEG","UGL","DIS","ZEN","MMQ","W17","LRW","DDI","pELP","C13","pWPN","SOM","DD3_GVL","PC2","V16","USG","CHR","pMEI","MMA" ]

-}

-- type AllSetNamesList = '["V13","ALA","DDL","5ED","C16","CST","MBS","CMA","ICE","E02","OGW","PD3","CEI","pPRE","DDG","M13","CM1","HOU","CON","DKM","V15","3ED","AKH","UST","MM2","PCA","CED","XLN","DDJ","pPRO","DDM","ITP","C17","ARN","pSUS","p15A","TSB","EXP","RAV","SOI","ATH","V12","LGN","pLPA","IMA","pALP","DRK","pWCQ","4ED","p2HG","LEA","CP3","MGB","9ED","M14","DDP","GTC","ARC","CHK","pGRU","ROE","DD3_EVG","V14","WTH","DTK","MM3","2ED","MRD","DDK","RIX","S99","BNG","DD3_JVC","DST","7ED","V11","pLGM","ULG","BOK","CNS","AER","MIR","DDN","C14","MPS","BFZ","EMN","DDQ","TOR","EMA","ARB","TMP","INV","HML","H09","ALL","8ED","CP2","VAN","M15","RTR","DDD","M10","ORI","DDT","KTK","EVG","ME4","E01","10E","ATQ","DRB","VIS","MED","pGTW","PTK","pGPX","DDO","SCG","STH","C15","HOP","6ED","CN2","5DN","PLS","pHHO","V09","THS","pARL","SOK","PLC","V10","TSP","pMPR","FUT","ONS","UDS","EXO","pCEL","PCY","BRB","CP1","BTD","FRF","ODY","CMD","DDR","DD3_DVD","DD2","APC","VMA","ME2","pWOR","pJGP","pDRC","POR","PO2","pFNM","DDE","M11","AVR","DPA","NMS","CSP","MOR","V17","RQS","W16","DDH","FRF_UGIN","MD1","JOU","WWK","DDC","GPT","pSUM","DDS","pREL","UNH","JUD","pPOD","ME3","pWOS","MPS_AKH","LEB","KLD","S00","NPH","ISD","DDF","M12","pMGD","pCMP","FEM","DGM","TPR","DKA","EVE","SHM","PD2","LEG","UGL","DIS","ZEN","MMQ","W17","LRW","DDI","pELP","C13","pWPN","SOM","DD3_GVL","PC2","V16","USG","CHR","pMEI","MMA"]

{-|


@
$ jq -c '{ LEA, ARN, ATQ, LEG, DRK, FEM, ICE, CHR, HML, ALL, MIR, VIS, POR, WTH, TMP, STH, PO2, EXO, USG, ULG, UDS, PTK, MMQ, NMS, PCY, INV, PLS, APC, ODY, TOR, JUD, ONS, LGN, SCG, MRD, DST, ("5DN"): .["5DN"], CHK, BOK, SOK, RAV, GPT, DIS, CSP, TSP, PLC, FUT, LRW, MOR, SHM, EVE, ALA, CON, ARB, M10, HOP, ZEN, WWK, ROE, M11, SOM, MBS, NPH, CMD, M12, ISD, DKA, PC2, AVR, M13, RTR, GTC, DDK, DGM, MMA, M14, THS, C13, BNG, JOU, CNS, M15, KTK, C14, FRF, DTK, ORI, BFZ, C15, OGW, W16, SOI, EMA, EMN, CN2, KLD, C16, AER, AKH, HOU, C17, XLN, RIX }' AllSets-x.json > RealSets-x.json

-- NOTE `("5DN"): .["5DN"]` has a numeric prefix, and thus can't be an identifier.
- 105 sets
@


@
$ jq '.[] |= {code, name, oldCode}' AllSets-x.json | grep -C 3 Portal
  "PTK": {
    "code": "PTK",
    "name": "Portal Three Kingdoms",
    "oldCode": null
  },
  "POR": {
    "code": "POR",
    "name": "Portal",
    "oldCode": null
  },
  "PO2": {
    "code": "PO2",
    "name": "Portal Second Age",
    "oldCode": "P02"
  },
@


@ $ jq '.[] |= {code, name, oldCode}' AllSets-x.json | grep -B3 '"oldCode": "'
  "pHHO": {
    "code": "pHHO",
    "name": "Happy Holidays",
    "oldCode": "HHO"
--
  "PO2": {
    "code": "PO2",
    "name": "Portal Second Age",
    "oldCode": "P02"
--
  "HOP": {
    "code": "HOP",
    "name": "Planechase",
    "oldCode": "PCH"
--
  "V12": {
    "code": "V12",
    "name": "From the Vault: Realms",
    "oldCode": "FVR"
--
  "V11": {
    "code": "V11",
    "name": "From the Vault: Legends",
    "oldCode": "FVL"
--
  "V09": {
    "code": "V09",
    "name": "From the Vault: Exiled",
    "oldCode": "FVE"
--
  "DD2": {
    "code": "DD2",
    "name": "Duel Decks: Jace vs. Chandra",
    "oldCode": "D2"
--
  "CMD": {
    "code": "CMD",
    "name": "Magic: The Gathering-Commander",
    "oldCode": "COM"
--
  "CM1": {
    "code": "CM1",
    "name": "Commander's Arsenal",
    "oldCode": "CMA"
--
  "NMS": {
    "code": "NMS",
    "name": "Nemesis",
    "oldCode": "NEM"
--
  "ITP": {
    "code": "ITP",
    "name": "Introductory Two-Player Set",
    "oldCode": "I2P"
--
  "CON": {
    "code": "CON",
    "name": "Conflux",
    "oldCode": "CFX"
--
  "9ED": {
    "code": "9ED",
    "name": "Ninth Edition",
    "oldCode": "9E"
--
  "8ED": {
    "code": "8ED",
    "name": "Eighth Edition",
    "oldCode": "8E"
@



@
LEA
ARN
2ED
ATQ
3ED
LEG
DRK
FEM
4ED
ICE
CHR
HML
ALL
MIR
VIS
5ED
POR
WTH
TMP
STH
PO2
EXO
USG
ULG
6ED
UDS
PTK
MMQ
NMS
PCY
INV
PLS
7ED
APC
ODY
TOR
JUD
ONS
LGN
SCG
8ED
MRD
DST
5DN
CHK
BOK
SOK
9ED
RAV
GPT
DIS
CSP
TSP
PLC
FUT
10E
LRW
MOR
SHM
EVE
ALA
CON
ARB
M10
HOP
ZEN
WWK
ROE
M11
SOM
MBS
NPH
CMD
M12
ISD
DKA
PC2
AVR
M13
RTR
GTC
DDK
DGM
MMA
M14
THS
C13
BNG
JOU
CNS
M15
KTK
C14
FRF
DTK
ORI
BFZ
PZ1
C15
OGW
W16
SOI
EMA
EMN
CN2
KLD
C16
AER
AKH
HOU
C17
XLN
RIX
@

-}
type RealSetNamesList =
 '[ "LEA"
  --LEB
  , "ARN"
--  , "2ED"
  , "ATQ"
--  , "3ED"
  , "LEG"
  , "DRK"
  , "FEM"
--  , "4ED"
  , "ICE"
  , "CHR"
  , "HML"
  , "ALL"
  , "MIR"
  , "VIS"
--  , "5ED"
  , "POR"
  , "WTH"
  , "TMP"
  , "STH"
  , "PO2"
  , "EXO"
  --UGL
  , "USG"
  , "ULG"
--  , "6ED"
  , "UDS"
  , "PTK"
  , "MMQ"
  , "NMS"
  , "PCY"
  , "INV"
  , "PLS"
--  , "7ED"
  , "APC"
  , "ODY"
  , "TOR"
  , "JUD"
  , "ONS"
  , "LGN"
  , "SCG"
--  , "8ED"
  , "MRD"
  , "DST"
  , "5DN"
  , "CHK"
  --UNH
  , "BOK"
  , "SOK"
--  , "9ED"
  , "RAV"
  , "GPT"
  , "DIS"
  , "CSP"
  , "TSP"
  , "PLC"
  , "FUT"
--  , "10E"
  , "LRW"
  , "MOR"
  , "SHM"
  , "EVE"
  , "ALA"
  , "CON"
  , "ARB"
  , "M10"
  , "HOP"
  , "ZEN"
  , "WWK"
  , "ROE"
  --ARC
  , "M11"
  , "SOM"
  , "MBS"
  , "NPH"
  , "CMD"
  , "M12"
  , "ISD"
  , "DKA"
  , "PC2"
  , "AVR"
  , "M13"
  , "RTR"
  , "GTC"
  , "DDK"
  , "DGM"
  , "MMA"
  , "M14"
  , "THS"
  , "C13"
  , "BNG"
  , "JOU"
  , "CNS"
  , "M15"
  , "KTK"
  , "C14"
  , "FRF"
  , "DTK"
  , "ORI"
  , "BFZ"
--  , "PZ1"
  , "C15"
  , "OGW"
  , "W16"
  , "SOI"
  , "EMA"
  , "EMN"
  , "CN2"
  , "KLD"
  , "C16"
  , "AER"
  , "AKH"
  , "HOU"
  , "C17"
  , "XLN"
  --UST
  , "RIX"
  --DOM
  --M19
  ]

{-





@
$ jq -c '{ LEA, ARN, 2ED, ATQ, 3ED, LEG, DRK, FEM, 4ED, ICE, CHR, HML, ALL, MIR, VIS, 5ED, POR, WTH, TMP, STH, P02, EXO, USG, ULG, 6ED, UDS, PTK, MMQ, NMS, PCY, INV, PLS, 7ED, APC, ODY, TOR, JUD, ONS, LGN, SCG, 8ED, MRD, DST, 5DN, CHK, BOK, SOK, 9ED, RAV, GPT, DIS, CSP, TSP, PLC, FUT, 10E, LRW, MOR, SHM, EVE, ALA, CON, ARB, M10, HOP, ZEN, WWK, ROE, M11, SOM, MBS, NPH, CMD, M12, ISD, DKA, PC2, AVR, M13, RTR, GTC, DDK, DGM, MMA, M14, THS, C13, BNG, JOU, CNS, M15, KTK, C14, FRF, DTK, ORI, BFZ, PZ1, C15, OGW, W16, SOI, EMA, EMN, CN2, KLD, C16, AER, AKH, HOU, C17, XLN, RIX }' AllSets-x.json > RealSets-x.json 
@




-}

----------------------------------------

{-| 


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

@

schema:

@
{
  "name"               : Unique 
                         // enforced implicitly via key
                         // field must be same as key
  "code"               : CodeSchema
  "gathererCode"       : CodeSchema
  "oldCode"            : CodeSchema
  "magicCardsInfoCode" : CodeSchema
  "releaseDate"        : Date   
  "border"             : BorderSchema            
  "type"               : SetTypeSchema                                 
  "block"              : BlockSchema           
  "onlineOnly"         : Bool          
  "booster"            : TOptional PackSchema
  "cards"              : TArray CardSchema  
}
@

e.g. value:

@
{
  "name" : "Nemesis"  ,          
  "code" : "NMS",                
  "gathererCode" : "NE",         
  "oldCode" : "NEM",             
  "magicCardsInfoCode" : "ne",   
  "releaseDate" : "2000-02-14"   
  "border" : "black",            
  "type" : "expansion",                                 
  "block" : "Masques",           
  "onlineOnly" : false,          
  "booster" : [ "rare", ... ],   
  "cards" : [ {}, {}, {}, ... ]  
}
@

e.g. value:

@
{
  "name" : "Nemesis",            // The name of the set
  "code" : "NMS",                // The set's abbreviated code
  "gathererCode" : "NE",         // The code that Gatherer uses for the set. Only present if different than 'code'
  "oldCode" : "NEM",             // An old style code used by some Magic software. Only present if different than 'gathererCode' and 'code'
  "magicCardsInfoCode" : "ne",   // The code that magiccards.info uses for the set. Only present if magiccards.info has this set
  "releaseDate" : "2000-02-14"   // When the set was released (YYYY-MM-DD). For promo sets, the date the first card was released.
  "border" : "black",            // The type of border on the cards, either "white", "black" or "silver"
  "type" : "expansion",          // Type of set. One of: "core", "expansion", "reprint", "box", "un",
                                 //                      "from the vault", "premium deck", "duel deck",
                                 //                      "starter", "commander", "planechase", "archenemy",
                                 //                      "promo", "vanguard", "masters", "conspiracy", "masterpiece"
  "block" : "Masques",           // The block this set is in,
  "onlineOnly" : false,          // Present and set to true if the set was only released online
  "booster" : [ "rare", ... ],   // Booster contents for this set, see below for details
  "cards" : [ {}, {}, {}, ... ]  // The cards in the set
}
@

-}
type SetSchema strictness = SchemaObject
 '[ "code"  :- SetCodeSchema strictness
  , "name"  :- SetNameSchema strictness 

  , "cards" :- CardsSchema   strictness

  --TODO the rest

  ]

----------------------------------------
  
type SetCodeSchema strictness = IF_STRICT strictness
 (SchemaText '[ TGe 3 ]) -- '[ TGe 3, TLe 4 ])
 -- SchemaUnion '[ SchemaText ['TEq 3 ...
 TEXT

type SetNameSchema strictness =
  TEXT

type CardsSchema strictness =
  ArraySchema (CardSchema strictness)

----------------------------------------

type BorderSchema strictness = ENUM_IF strictness
 Borders

type SetTypeSchema strictness = ENUM_IF strictness
 SetTypes

type BlockSchema strictness = ENUM_IF strictness
 Blocks

----------------------------------------

type PackSchema strictness = ArraySchema (BoosterSchema strictness)

type BoosterSchema strictness = EitherSchema
  (RaritySchema strictness)
  (ArraySchema (RaritySchema strictness))

{- 

@
-- Shared
common
uncommon
rare
mythic rare
land
marketing
checklist
double faced

-- Time Spiral block
timeshifted common
timeshifted uncommon
timeshifted rare
timeshifted purple

-- "Conspiracy" expansion
draft-matters
-- either a Conspiracy-type card, or a card that affects drafting. 

-- "Vintage Masters"
power nine
foil

-- Modern Masters
foil common
foil uncommon
foil rare
foil mythic rare
-- Foils are implicit, except in Modern Masters

-- starter
any { "starter" : true } is exempted from any booster generation (as these cards were only available in boxed sets and not in boosters).
@

i.e.

 , "common"
 , "uncommon"
 , "rare"
 , "mythic rare"
 , "land"
 , "marketing"
 , "checklist"
 , "double faced"

-- Time Spiral 
 , "timeshifted common"
 , "timeshifted uncommon"
 , "timeshifted rare"
 , "timeshifted purple"

-- Conspiracy
 , "draft-matters"

-- Vintage Masters
 , "power nine"
 , "foil"

-- Modern Masters
 , "foil common"
 , "foil uncommon"
 , "foil rare"
 , "foil mythic rare"

-}
type RaritySchema strictness = ENUM_IF strictness
  Rarities

----------------------------------------

{-| 

@
id,name,manaCost,cmc,colors,colorIdentity,type,rarity,text,flavor,power,toughness,loyalty,artist,mciNumber,rulings,legalities
@

@
$ jq '.[] |= {code, name, cards: .cards | map({ name,id,manaCost,cmc,colors,colorIdentity,type,rarity,text,flavor,power,toughness,loyalty,artist,mciNumber,rulings,legalities })}' RealSets-x.json > RealSets-y.json
@


@

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
type CardSchema strictness = SchemaObject
 '[ "id"            :- SHA1Schema              strictness

  , "name"          :- CardNameSchema          strictness

  , "manaCost"      :- ManaCostSchema          strictness
  , "cmc"           :- ConvertedManaCostSchema strictness

  , "colors"        :- ColorSchema             strictness
  , "colorIdentity" :- ColorIdentitySchema     strictness

  , "type"          :- TypelineSchema       strictness
  , "types"         :- TypesSchema          strictness
  , "supertype"     :- TypesSchema          strictness
  , "subtype"       :- TypesSchema          strictness

  , "rarity"        :- RaritySchema            strictness

  , "text"          :- OracleTextSchema        strictness
  , "flavor"        :- FlavorTextSchema        strictness

  , "power"         :- PowerSchema             strictness
  , "toughness"     :- ToughnessSchema         strictness
  , "loyalty"       :- LoyaltySchema           strictness

  , "artist"        :- ArtistSchema            strictness

  , "mciNumber"     :- MCISchema               strictness

  , "rulings"       :- RulingsSchema           strictness  
--  , "legalities"    :- LegalitiesSchema        strictness
--  , "foreignNames"  :- ForeignPrintingsSchema  strictness
  ]  

----------------------------------------

{-| SHA1, as a hexadecimal number, is 40 digits long.
-}
type SHA1Schema strictness = TEXT_UNLESS strictness (HexSchema 40)

type MCISchema strictness = SchemaOptional TEXT

----------------------------------------

{-| Mostly ASCII alphanumerics; some older cards have Unicode.

i.e. afaik, no new card names with non-ASCII symbols will be introduced; or at least, none with new Unicode symbols.

and, some Unicode symbols (like the "ae" in "Aether") have already been deprecated.

-}
type CardNameSchema strictness = REGEX_IF 'Loose --TODO strictness
  "[0-9a-zA-Z\\.,]*"
   -- TextConstraint '[TRegex ""]

----------------------------------------

type ManaCostSchema strictness =
  SchemaOptional (ManaSymbolSchema strictness)
  --  SchemaOptional (SchemaArray '[] (ManaSymbolSchema strictness))
  -- ERROR
  --  DecodingError "Error in $: expected Array, encountered String"
  
type ConvertedManaCostSchema strictness =
  NUMBER 

----------------------------------------

type TypesSchema strictness =
  SchemaArray '[] TEXT

type TypelineSchema strictness =
  TEXT

--type ForeignPrintingsSchema strictness = ()

----------------------------------------

type OracleTextSchema strictness = SchemaOptional TEXT
type FlavorTextSchema strictness = SchemaOptional TEXT

type ArtistSchema strictness = TEXT

----------------------------------------

type PowerSchema strictness     =
  SchemaOptional (StatSchema strictness)
type ToughnessSchema strictness =
  SchemaOptional (StatSchema strictness)

type LoyaltySchema strictness = SchemaOptional
  (IF_STRICT strictness
   (REGEX "[0-9]+")
   NUMBER)

----------------------------------------

{-| 

-}
type ColorSchema strictness = SchemaArray '[]
 (ENUM_IF strictness
  ColorNames)

{-| 

-}
type ColorIdentitySchema strictness = SchemaArray '[]
 (ENUM_IF strictness
  ColorSymbols)

----------------------------------------

{-| 

-}
type RulingsSchema strictness = SchemaOptional (SchemaArray '[]
  (RulingSchema strictness))

----------------------------------------
  
-- {-| 

-- -}
-- type LegalitiesSchema strictness = ENUM_IF strictness
--  Legalities

----------------------------------------

type StatSchema strictness = TEXT_UNLESS strictness
  (REGEX "[\\-0-9\\*\\+]+/[\\-0-9\\*\\+]+")

----------------------------------------

{-| 

-}
type ManaSymbolSchema strictness = IF_STRICT strictness
 TEXT -- TODO
 -- (SchemaArray ManaSymbols StaticManaSymbols)
 TEXT

{-| 

-}
type ColorSymbolSchema strictness = ENUM_IF strictness
  ColorSymbols

----------------------------------------

{-| 

-}
type SupertypeSchema strictness = ENUM_IF strictness
  Supertypes

{-| 

-}
type TypeSchema strictness = ENUM_IF strictness
  Types


-- {-| NOTE changes with most sets.

-- most sets introduce several new creature types.

-- -}
-- type SubtypeSchema strictness = SchemaObject --TODO
--  '[ "Creature" :- CreatureSubtypeSchema strictness
--   , "Instant"  :- SpellSubtypeSchema    strictness
--   ]

{-| 

-}
type CreatureSubtypeSchema strictness = ENUM_IF strictness
 CreatureSubtypes

type SpellSubtypeSchema strictness = ENUM_IF strictness
 SpellSubtypes

----------------------------------------
 
{-| 

-}
type LanguagesSchema strictness = ENUM_IF strictness
 Languages

---------------------------------------

{-| 

-}
type RulingSchema strictness = SchemaObject
 '[ "date" :- DateSchema strictness
  , "text" :- TEXT
  ]
 
{-| 

-}
type DateSchema strictness = REGEX_IF strictness
 ISO8601Date
  
---------------------------------------
-- LISTS
---------------------------------------

type Borders =
 '[ "white"
  , "black"
  , "silver"
  ]

type SetTypes =
 '[ "core"
  , "expansion"
  , "reprint"
  , "box"
  , "un"
  , "from the vault"
  , "premium deck"
  , "duel deck"
  , "starter"
  , "commander"
  , "planechase"
  , "archenemy"
  , "promo"
  , "vanguard"
  , "masters"
  , "conspiracy"
  , "masterpiece"
  ]

{-| NOTE changes with most sets. 

-}
type Blocks = --TODO
 '[
  ]

type Rarities =
 '[ "common"
  , "uncommon"
  , "rare"
  , "mythic rare"
  , "land"
  , "marketing"
  , "checklist"
  , "double faced"
 
    -- Time Spiral 
  , "timeshifted common"
  , "timeshifted uncommon"
  , "timeshifted rare"
  , "timeshifted purple"
 
    -- Conspiracy
  , "draft-matters"
 
    -- Vintage Masters
  , "power nine"
  , "foil"
 
    -- Modern Masters
  , "foil common"
  , "foil uncommon"
  , "foil rare"
  , "foil mythic rare"
  ]

{- TEMPLATE

{-| 

-}
type Schema strictness = ENUM_IF strictness
 '[ 
  ]

-}

----------------------------------------

{-| 

-}
type ColorNames =
 '[ "White"
  , "Blue"
  , "Black"
  , "Red"
  , "Green"
  ]

{-| 

-}
type ManaSymbols = NumericalManaSymbols ++ StaticManaSymbols

{-| 

-}
type NumericalManaSymbols = --TODO? Regex
 '[ "{0}"  
  , "{1}"
  , "{2}"
  , "{3}"
  , "{4}"
  , "{5}"
  , "{6}"
  , "{7}"
  , "{8}"
  , "{9}"
  , "{10}"
  , "{11}"
  , "{12}"
  , "{13}"
  , "{14}"
  , "{15}"
  , "{16}"
  , "{17}"
  , "{18}"
  , "{19}"
  , "{20}"
  ]

{-| 

-}
type StaticManaSymbols = --TODO finish
 '[ "{W}"
  , "{U}"
  , "{B}"
  , "{R}"
  , "{G}"
  , "{X}"
--  , "{Y}"
  ] 

-- {-| 

-- -}
-- type ManaSymbols =
--  '[ "W"
--   , "U"
--   , "B"
--   , "R"
--   , "G"
--   ]

{-| 

-}
type ColorSymbols = 
 '[ "W"
  , "U"
  , "B"
  , "R"
  , "G"
  ] 

-------------------------------------------

{-| 

-}
type Supertypes =  
 '[ "Basic"
  , "Host"
  , "Legendary"
  , "Ongoing"
  , "Snow"
  , "World"
  ]
 
{-| 

-}
type Types =
  '[ "Artifact"
   , "Creature"
   , "Enchantment"
   , "Instant"
   , "Land"
   , "Planeswalker"
   , "Sorcery"
   , "Tribal"
   --
   , "Plane"
   , "Phenomenon"
   , "Vanguard"
   , "Scheme"
   , "Conspiracy"
   ]
 
{-| 

-}
type SubtypesOf = 
  '[ "Artifact"     :- ArtifactSubtypes
   , "Enchantment"  :- EnchantmentSubtypes
   , "Land"         :- LandSubtypes
   , "Instant"      :- SpellSubtypes
   , "Sorcery"      :- SpellSubtypes
   , "Creature"     :- CreatureSubtypes
   , "Planeswalker" :- PlaneswalkerSubtypes
   ]

{-| 

-}
type ArtifactSubtypes = 
  '[ "Clue"
   , "Contraption"
   , "Equipment"
   , "Fortification"
   , "Treasure"
   , "Vehicle"
   ]

{-| 

-}
type EnchantmentSubtypes = 
  '[ "Aura"
   , "Cartouche"
   , "Curse"
   , "Shrine"
   ]

{-| 

-}
type LandSubtypes = 
  '[ "Plains"
   , "Island"
   , "Swamp"
   , "Mountain"
   , "Forest"
   , "Desert"
   , "Gate"
   , "Lair"
   , "Locus"
   , "Urza's"
   , "Mine"
   , "Power-Plant"
   , "Tower"
   ]

{-| 

-- Instant/Sorcery

-}
type SpellSubtypes = 
  '[ "Arcane"
   , "Trap"
   ]

{-| 


-}
type PlaneswalkerSubtypes =
 '[

  ]

{-| 

"THE GRAND CREATURE TYPE UPDATE"

-}
type CreatureSubtypes = 
 '[
  ]

---------------------------------------

{-|
-}
type Legalities =
 '[ "Legal" 
  , "Restricted" 
  , "Banned" 
  ]

{-|
-}
type Languages =
 '[ "English"
  , "Chinese Simplified"
  , "Chinese Traditional"
  , "French"
  , "German"
  , "Italian"
  , "Japanese"
  , "Korean"
  , "Portuguese"
  , "Russian"
  , "Spanish"
  ]

 -- '[ "en"
 --  , "es"
 --  ]

----------------------------------------

type SchemaDecoder schema
  = ByteString
 -> ParseResult (JsonRepr schema)

decodeSimpleSetsSchemaStrictly
  :: ByteString
  -> ParseResult (JsonRepr (SetsSchema 'Strict)) 
decodeSimpleSetsSchemaStrictly = decodeAndValidateJson

decodeSimpleSetsSchemaLoosely
  :: ByteString
  -> ParseResult (JsonRepr (SetsSchema 'Loose)) 
decodeSimpleSetsSchemaLoosely = decodeAndValidateJson

----------------------------------------

{-

@
Valid ReprObject {FieldRepr ReprArray [ReprNumber 13.0], FieldRepr ReprOptional Just ReprText "bar"}

ValidationError (MonoidMap (fromList [(".foo[0]",[".foo[0] should be > 10"])]))

ReprObject {FieldRepr ReprArray [ReprNumber 12.0], FieldRepr ReprOptional Just ReprText "bar"}

ReprObject {FieldRepr ReprArray [ReprNumber 12.0], FieldRepr ReprOptional Just ReprText "bar"}
@

-}
main :: IO ()
main = do
  putStrLn ""
  let jGood = decodeAndValidateJson jsonExample_raw :: ParseResult (JsonRepr SchemaExample)
  print jGood

  putStrLn ""
  let jBad = decodeValidExampleSchema jsonExample_bad 
  print jBad

  putStrLn ""
  print jsonExample_sugar

  putStrLn ""
  print jsonExample_sugarless

  putStrLn ""
  -- p'RIX <- getDataFile "data/json/XLN-RIX-y.json" -- path
  -- s'RIX <- B.readFile p'RIX -- string

  -- s'RIX <- readRIXSets -- bytestring
  -- let j'RIX'loose  = decodeSimpleSetsSchemaLoosely  s'RIX -- json
  -- let j'RIX'strict = decodeSimpleSetsSchemaStrictly s'RIX -- json

  s'RIX <- readRIXSets -- bytestring
  let j'RIX'loose  = decodeRIXLoosely  s'RIX -- json
  let j'RIX'strict = decodeRIXStrictly s'RIX -- json

  putStrLn ""
  putStrLn "[loose RIX]"
  putStrLn ""
  print j'RIX'loose

  putStrLn ""
  putStrLn "[strict RIX]"
  putStrLn ""
  print j'RIX'strict

---------------------------------------

type RIX_SetsSchema strictness = SchemaObject
  (HomogeneousObject
   '["RIX","XLN"]
   (SetSchema strictness))

-- type RIX_SetsSchema strictness = SchemaObject
--  (UNLESS_STRICT strictness '[]
--   (HomogeneousObject '["RIX","XLN"]
--                      (SetSchema strictness)))

decodeRIXStrictly :: SchemaDecoder (RIX_SetsSchema 'Strict)
decodeRIXStrictly = decodeAndValidateJson

decodeRIXLoosely ::  SchemaDecoder (RIX_SetsSchema 'Loose)
decodeRIXLoosely = decodeAndValidateJson

-- fpAllSetsX :: FilePath
-- fpAllSetsX = "data/json/AllSets-x.json"  

-- fpRealSetsX :: FilePath
-- fpRealSetsX = "data/json/RealSets-x.json"

{-| RIX-block sets, as a "homogeneous" JSON Object. -}
fpRIXSets :: FilePath
fpRIXSets = "data/json/RIXSets-y.json"

-- {-| RIX-block sets, as a JSON Array. -}
-- fpRIXSetsArray :: FilePath
-- fpRIXSetsArray = "data/json/RIXSets-z.json"

readRIXSets :: IO B.ByteString
readRIXSets = getDataFileName fpRIXSets >>= B.readFile 

----------------------------------------

type SchemaExample =
 SchemaObject
  [ "foo" :- SchemaArray '[AEq 1] (SchemaNumber '[NGt 10])
  , "bar" :- SchemaOptional (SchemaText '[TEnum '["foo", "bar"]])
  ]

  -- = SchemaObject
  --   '[ '("foo", SchemaArray '[AEq 1] (SchemaNumber '[NGt 10]))
  --    , '("bar", SchemaOptional (SchemaText '[TEnum '["foo", "bar"]]))]

----------------------------------------

decodeValidExampleSchema :: ByteString -> ParseResult (JsonRepr SchemaExample) 
decodeValidExampleSchema = decodeAndValidateJson 

----------------------------------------

jsonExample_raw :: ByteString
jsonExample_raw = "{\"foo\": [13], \"bar\": \"bar\"}"

jsonExample_bad :: ByteString
jsonExample_bad = "{\"foo\": [10], \"bar\": \"bar\"}"

jsonExample_sugar :: JsonRepr SchemaExample
jsonExample_sugar = withRepr @SchemaExample
    $ field @"foo" [12]
   :& field @"bar" (Just "bar")
   :& RNil

jsonExample_sugarless :: JsonRepr SchemaExample
jsonExample_sugarless = ReprObject 
     $ FieldRepr (ReprArray [ReprNumber 12])
    :& FieldRepr (ReprOptional (Just (ReprText "bar")))
    :& RNil

----------------------------------------

{-TEMPLATES

{-| 
-}
type Schema = ENUM
 '[ 
  ]


-}