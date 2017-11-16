{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings  #-}
module Card.Core where
import Card.Extra
import Card.Types() 
import Paths_magic_card_search 

import Data.Aeson 

import Prelude.Spiros

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8 
import qualified Data.Map as M 
import Data.Binary
import Data.String (fromString) 

import Control.Concurrent
import Control.Concurrent.Async.Pool
import Network.HTTP.Client
import Network.HTTP.Client.TLS

data CardIdentifier = CardIdentifier 
  { cardLanguage :: Language  -- TODO 
  , cardSet      :: MagicSet 
  , cardCollectorNumber :: CollectorNumber  
  }

type Language = String 

type MagicSet = String 

type CollectorNumber = Natural 

defaultCardIdentifier :: MagicSet -> CollectorNumber -> CardIdentifier 
defaultCardIdentifier cardSet cardCollectorNumber = CardIdentifier{..}
  where 
  cardLanguage = "en" 

saveImagesFromMagicCardsInfo :: Natural -> [CardIdentifier] -> IO () 
saveImagesFromMagicCardsInfo t cs = do 
  manager <- newManager tlsManagerSettings
  traverse_ (go manager) (reverse cs) 
  where 
  go manager c = do
    putStrLn $ urlFromCardIdentifier c 
    putStrLn $ pathFromCardIdentifier c 
    i <- downloadImageFromMagicCardsInfo manager c 
    B.writeFile (pathFromCardIdentifier c) i
    delayMilliseconds (t&fromIntegral)  -- threadDelay (fromIntegral t) 
  
downloadImageFromMagicCardsInfo :: Manager -> CardIdentifier -> IO B.ByteString
downloadImageFromMagicCardsInfo manager c = do 
  request <- parseUrlThrow url
  response <- httpLbs request manager
  let body = response&responseBody 
  return body 
  where 
  url = urlFromCardIdentifier c 

{-| e.g. @@ 

-}
pathFromCardIdentifier :: CardIdentifier -> FilePath 
pathFromCardIdentifier CardIdentifier{..} 
  = "data/images/" <> cardSet <> "-" <> cardLanguage <> "/" <> show cardCollectorNumber <> ".jpg" 

{-| e.g. @https://magiccards.info/scans/en/xln/1.jpg@ 

-}
urlFromCardIdentifier :: CardIdentifier -> String 
urlFromCardIdentifier CardIdentifier{..} 
  = "https://magiccards.info/scans/"<> cardLanguage<>"/" <> cardSet <>"/"<> (show cardCollectorNumber)<>".jpg" 
--   = fromString $ "https://magiccards.info/scans/"<> fromString cardLanguage<>"/" <> fromString cardSet <>"/"<>fromString (show cardCollectorNumber)<>".jpg" 
  
-- = withManager tlsManagerSettings $ \mgr ->
--     withTaskGroup 2 $ \tg -> mapConcurrently tg (\url -> do
--         req <- parseUrl url
--         responseBody <$> httpLbs req mgr <* threadDelay 1) urls

cardFile :: FilePath
cardFile = "data/SomeCards.json"

-- encodeFile
-- decodeFile

getCards :: IO B.ByteString
getCards = getDataFileName cardFile >>= B.readFile 

printCards = getCards >>= B8.putStrLn

parseCards :: B.ByteString -> Either String Value  
parseCards b = eitherDecode b 

indexCards :: B.ByteString -> Either String (M.Map T.Text Value)
indexCards b = eitherDecode b <&> (M.mapKeys T.toCaseFold >>> M.filter (const True)) 

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
