{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, TypeApplications #-}
module Card.Core where
import Card.Extra
import Card.Types
import Card.Schema 
import Paths_magic_card_search 

import Data.Aeson 

import Prelude.Spiros() 

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString.Lazy.Char8 as B8 
import qualified Data.Map as M 
-- import Data.Binary
-- import Data.String (fromString) 
import Control.Lens hiding ((<&>)) 
import Data.Aeson.Lens

-- import Control.Concurrent
-- import Control.Concurrent.Async.Pool
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception(handle) 

{-| 

@
{
  "LEA" : { /* set data */ },
  "LEB" : { /* set data */ },
  "2ED" : { /* set data */ },
  ...
}

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

"core", "expansion", "reprint", "commander", "planechase", "archenemy", masters", "conspiracy"

layout: normal, split, flip, double-faced, leveler, meld

names: Only used for split, flip, double-faced, and meld cards. Will contain all the names on this card, front or back. For meld cards, the first name is the card with the meld ability, which has the top half on its back, the second name is the card with the reminder text, and the third name is the melded back face.

The card colors. Usually this is derived from the casting cost, but some cards are special (like the back of double-faced cards and Ghostfire)

The card number. This is printed at the bottom-center of the card in small text. This is a string, not an integer, because some cards have letters in their numbers.

The power of the card. This is only present for creatures. This is a string, not an integer, because some cards have powers like: "1+*"

The multiverseid of the card on Wizard's Gatherer web page.
Cards from sets that do not exist on Gatherer will NOT have a multiverseid.
Sets not on Gatherer are: ATH, ITP, DKM, RQS, DPA 

variations : If a card has alternate art (for example, 4 different Forests, or the 2 Brothers Yamazaki) then each other variation's multiverseid will be listed here

Note: Split cards don't currently have this field set, despite having a watermark on each side of the split card

mciNumber 109: Number used by MagicCards.info for their indexing URLs (Most often it is the card number in the set)

rulings	[ { "date" : "2003-04-15",
"text" : "Does not tap." } ]	The rulings for the card. An array of objects, each object having 'date' and 'text' keys.
foreignNames	[ { "language" : "Italian",
"name" : "Wurm Devastatore",
"multiverseid" : 200475 } ]	Foreign language names for the card, if this card in this set was printed in another language. An array of objects, each object having 'language', 'name' and 'multiverseid' keys. Not available for all sets.
printings	[ "ICE", "CHR" ]	The sets that this card was printed in, expressed as an array of set codes.
originalText	"{8}: Do 4 damage to any target."	The original text on the card at the time it was printed. This field is not available for promo cards.
originalType	"Mono Artifact"	The original type on the card at the time it was printed. This field is not available for promo cards.
legalities	[ { "format" : "Legacy",
"legality" : "Banned" },
{ "format" : "Vintage",
"legality" : "Restricted" } ]	Which formats this card is legal, restricted or banned in. An array of objects, each object having 'format' and 'legality'. A 'condition' key may be added in the future if Gatherer decides to utilize it again.


@
Object (fromList [("mciNumber",String "229"),("layout",String "normal"),("text",String "Vivid Meadow enters the battlefield tapped with two charge counters on it.\n{T}: Add {W} to your mana pool.\n{T}, Remove a charge counter from Vivid Meadow: Add one mana of any color to your mana pool."),("colorIdentity",Array [String "W"]),("multiverseid",Number 370351.0),("types",Array [String "Land"]),("originalType",String "Land"),("legalities",Array [Object (fromList [("format",String "Commander"),("legality",String "Legal")]),Object (fromList [("format",String "Legacy"),("legality",String "Legal")]),Object (fromList [("format",String "Lorwyn-Shadowmoor Block"),("legality",String "Legal")]),Object (fromList [("format",String "Modern"),("legality",String "Legal")]),Object (fromList [("format",String "Vintage"),("legality",String "Legal")])]),("originalText",String "Vivid Meadow enters the battlefield tapped with two charge counters on it.\n{T}: Add {W} to your mana pool.\n{T}, Remove a charge counter from Vivid Meadow: Add one mana of any color to your mana pool."),("name",String "Vivid Meadow"),("printings",Array [String "LRW",String "CMD",String "MMA",String "C15",String "CMA",String "C17"]),("rarity",String "Uncommon"),("cmc",Number 0.0),("id",String "2e34dd2394d5d1b8386280ca1d507587e10d6bdd"),("imageName",String "vivid meadow"),("type",String "Land"),("number",String "229"),("artist",String "Rob Alexander")])]),("code",String "MMA"),("magicCardsInfoCode",String "mma"),("releaseDate",String "2013-06-07"),("type",String "reprint")]))]
@

-}



{-| magiccards.info

-}
data MCICardIdentifier = MCICardIdentifier 
  { cardLanguage        :: Language  -- TODO 
  , cardSet             :: MagicSetCode 
  , cardCollectorNumber :: CollectorNumber  
  }

type Language = String 

type MagicSetCode = String 

type CollectorNumber = Natural 

defaultMCICardIdentifier :: MagicSetCode -> CollectorNumber -> MCICardIdentifier 
defaultMCICardIdentifier cardSet cardCollectorNumber = MCICardIdentifier{..}
  where 
  cardLanguage = "en" 

saveImagesFromMagicCardsInfo :: Natural -> [MCICardIdentifier] -> IO () 
saveImagesFromMagicCardsInfo t cs = do 
  manager <- newManager tlsManagerSettings
  traverse_ (go manager) (reverse cs) 
  where 
  go manager c = do
    putStrLn $ urlFromMCICardIdentifier c 
    putStrLn $ pathFromMCICardIdentifier c 
    downloadImageFromMagicCardsInfo manager c >>= either failure (success c) 
  failure e = print e 
  success c i = do 
    B.writeFile (pathFromMCICardIdentifier c) i
    delayMilliseconds (t&fromIntegral)  -- threadDelay (fromIntegral t) 
  
downloadImageFromMagicCardsInfo :: Manager -> MCICardIdentifier -> IO (Either HttpException B.ByteString)
downloadImageFromMagicCardsInfo manager c = handleHttpErrors $ do 
  request <- parseUrlThrow url
  response <- httpLbs request manager
  let body = response&responseBody 
  return $ Right body 
  where 
  url = urlFromMCICardIdentifier c
  handleHttpErrors = handle @HttpException (Left > return) 
  -- handle $ (\(e ::HttpException) -> Left e) 

{-| e.g. @data/images/xln-en/1.jpg@ 

-}
pathFromMCICardIdentifier :: MCICardIdentifier -> FilePath 
pathFromMCICardIdentifier MCICardIdentifier{..} 
  = "data/images/" <> cardSet <> "-" <> cardLanguage <> "/" <> show cardCollectorNumber <> ".jpg" 

{-| e.g. @https://magiccards.info/scans/en/xln/1.jpg@ 

-}
urlFromMCICardIdentifier :: MCICardIdentifier -> String 
urlFromMCICardIdentifier MCICardIdentifier{..} 
  = "https://magiccards.info/scans/"<> cardLanguage<>"/" <> cardSet <>"/"<> (show cardCollectorNumber)<>".jpg" 
--   = fromString $ "https://magiccards.info/scans/"<> fromString cardLanguage<>"/" <> fromString cardSet <>"/"<>fromString (show cardCollectorNumber)<>".jpg" 
  
-- = withManager tlsManagerSettings $ \mgr ->
--     withTaskGroup 2 $ \tg -> mapConcurrently tg (\url -> do
--         req <- parseUrl url
--         responseBody <$> httpLbs req mgr <* threadDelay 1) urls

----

exampleSingleCardfile :: FilePath
exampleSingleCardfile = "data/OneCard-x.json" -- 

defaultSetsFile :: FilePath
defaultSetsFile = "data/AllSets-x.json"  

getOneSet :: IO B.ByteString
getOneSet = getDataFileName "data/OneSet-x.json" >>= B.readFile 

-- encodeFile
-- decodeFile

getSomeSets  :: IO B.ByteString
getSomeSets = getDataFileName "data/SomeSets-x.json" >>= B.readFile 

getSetsDefault :: IO B.ByteString
getSetsDefault = getDataFileName defaultSetsFile >>= B.readFile 

parseSets :: B.ByteString -> Either String Value  
parseSets b = eitherDecode b 

indexSets :: B.ByteString -> Either String (M.Map T.Text Value)
indexSets b = eitherDecode b <&> (M.mapKeys T.toCaseFold >>> M.filter (const True)) 

type MagicSet = Value -- M.Map T.Text Value
type MagicCard = Value 

rCardNames_example :: Traversal' MagicSet T.Text -- MagicCard   
rCardNames_example = (key "LEA" . key "cards" . values . key "name" . _String )
-- rCardNames = (key "lea" . key "cards" . _Object . key "name")
 
{-| filter sets for validity. 
for each valid set, for each of its cards, pair it it's collectors number. 

-}


---

defaultCardsFile :: FilePath
defaultCardsFile = "data/AllCards.json" -- "data/SomeCards.json"

-- encodeFile
-- decodeFile

getOneCard :: IO B.ByteString
getOneCard = getDataFileName "data/OneCard-x.json" >>= B.readFile 

getCardsDefault :: IO B.ByteString
getCardsDefault = getDataFileName defaultCardsFile >>= B.readFile 

parseCards :: B.ByteString -> Either String Value  
parseCards b = eitherDecode b 

indexCards :: B.ByteString -> Either String (M.Map T.Text Value)
indexCards b = eitherDecode b <&> (M.mapKeys T.toCaseFold >>> M.filter (const True)) 

---

readUntilEmpty :: (T.Text -> IO ()) -> IO ()
readUntilEmpty f = go
 where 
  go = do
    t <- T.getLine  
    if T.null t 
    then nothing 
    else do
       f t
       go

{- | on Windows, some applications like Notepad obnoxiously prefix the file contents with a Byte Order Mark (I think). 

@
∩╗┐{...}
@

https://en.wikipedia.org/wiki/Byte_order_mark

https://github.com/bos/aeson/issues/389

@utf8_bom :: TextEncoding@
The UTF-8 Unicode encoding, with a byte-order-mark (BOM; the byte sequence 0xEF 0xBB 0xBF). This encoding behaves like utf8, except that on input, the BOM sequence is ignored at the beginning of the stream, and on output, the BOM sequence is prepended.
The byte-order-mark is strictly unnecessary in UTF-8, but is sometimes used to identify the encoding of a file.

@
> [0xEF,0xBB,0xBF]
[239,187,191]
@ 

-}
stripByteOrderMark :: B.ByteString -> B.ByteString
stripByteOrderMark b = B.drop 3 b

--------------------------------------------------------------------------------

validateSet :: SetObject -> Either String SetData 
validateSet _o = Left "" 

validateCard :: CardObject -> Either String CardData 
validateCard _o = Left "" 