{-# LANGUAGE NoImplicitPrelude #-}
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

cardFile :: FilePath
cardFile = "data/AllCards.json"

getCards :: IO B.ByteString
getCards = getDataFileName cardFile >>= B.readFile 

printCards = getCards >>= B8.putStrLn

parseCards :: B.ByteString -> Either String Value  
parseCards b = eitherDecode b 

indexCards :: B.ByteString -> Either String (M.Map T.Text Value)
indexCards b = eitherDecode b <&> M.mapKeys T.toCaseFold 

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
