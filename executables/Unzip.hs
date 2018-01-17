{-# LANGUAGE PackageImports, TypeApplications, RecordWildCards, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.Foldable
import Control.Arrow

import qualified "zip-archive" Codec.Archive.Zip as ZIP

import qualified Codec.Compression.GZip     as GZIP    -- GZIP format 
import qualified Codec.Compression.Zlib     as ZLIB    -- ZLIB format 
import qualified Codec.Compression.Zlib.Raw as DEFLATE -- DEFLATE format 
import qualified Codec.Compression.Zlib.Raw as Z

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.ByteString.Lazy (ByteString)

import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Client.TLS   as HTTPS
import qualified Network.HTTP.Types.Status as HTTP 
import qualified Network.Wreq              as W

import qualified Control.Lens    as L
import qualified Data.Aeson.Lens as J

-- import Control.Concurrent
-- import Control.Concurrent.Async

import Control.Exception
import System.Environment (getArgs)
import Data.Monoid
import Control.Monad
import Data.Maybe

import Control.Concurrent

--------------------------------------------------------------------------------

{-|

@
http://mtgjson.com/json/AllSets-x.json.zip
https://mtgjson.com/json/AllSets-x.json.zip
@ 


@
stack build :get-magic-cards-json && stack exec -- get-magic-cards-json
stack build :get-magic-cards-json ;  stack exec -- get-magic-cards-json
@

PROBLEM 
    Codec.Compression.Zlib: compressed data stream format error (incorrect header check)
SOLUTION
   https://stackoverflow.com/questions/35683896/decompress-zlib-data-in-haskell-incorrect-header-check
   use Codec.Compression.Zlib.Raw

ZIP is an archive file format that supports lossless data compression. A .ZIP file may contain one or more files or directories that may have been compressed. The .ZIP file format permits a number of compression algorithms, though DEFLATE is the most common.

Codec.Compression.GZip      -- GZIP format 
Codec.Compression.Zlib      -- ZLIB format 
Codec.Compression.Zlib.Raw  -- DEFLATE format 

PROBLEM 
    Codec.Compression.Zlib: compressed data stream format error (invalid stored block lengths)
SOLUTION

7Zip says that AllSets-x.json.zip has no errors:
method = deflate
version = 20
(96MB uncompressed (it's 12MB compressed))
(CRC = 165E620C)

zlib "invalid stored block lengths"


-}

main = main'

--------------------------------------------------------------------------------

printingSomeException e = do
  putStrLn ""
  putStrLn "**************************************************"
  putStrLn $ displayException @SomeException e
  putStrLn "**************************************************"

(-:) = (,)
infixr 1 -:

nothing = pure ()

pause t = threadDelay (t*1000)

--------------------------------------------------------------------------------

{-

main = do
 [0..8] & traverse_ mainWith
compile

mainWith k = handle printingSomeException $ do
  z <- B.readFile "data/zipped/AllSets-x.json.zip"
  let z' = z & B.drop k
  putStrLn $ "decompressing..." ++ " (dropping "++(show k)++ " bytes)" 
  let j = Z.decompressWith decompressionParameters z'
  B.writeFile ("data/zipped/AllSets-x."++(show k)++".json") j
  putStrLn "written."

  -- where
-- decompressionParameters = Z.defaultDecompressParams

decompressionParameters = Z.DecompressParams{..}
  where 
  decompressWindowBits = Z.windowBits 15
  decompressBufferSize = 100 * 1024 * 1024  -- 100MB
  decompressDictionary = Nothing 
  decompressAllMembers = False 

printingSomeException = displayException @SomeException >>> putStrLn 



mainWith x = handle printingSomeException $ do
  putStrLn ""
  putStrLn $ show x
  z <- B.readFile "data/zipped/AllSets-x.json.zip"
  let z' = z 
  putStrLn $ "decompressing..."
  let j = Z.decompressWith decompressionParameters z'
  B.writeFile ("data/zipped/AllSets-x.json") j
  let l = B.length j
  putStrLn $ "written (" ++ (show l) ++ " bytes)."

-- decompressionParameters = Z.defaultDecompressParams
decompressionParameters = Z.DecompressParams{..}
  where 
  decompressWindowBits = Z.windowBits 15
  decompressBufferSize = 100 * 1024 * 1024  -- 100MB
  decompressDictionary = Nothing 
  decompressAllMembers = False 



mainWith x = handle printingSomeException $ do
  putStrLn ""
  putStrLn $ show x
  z <- B.readFile "data/zipped/AllSets-x.json.zip"
  let z' = z 
  putStrLn $ "decompressing..."
  let j = Z.decompressWith decompressionParameters z'
  B.writeFile ("data/zipped/AllSets-x.json") j
  let l = B.length j
  putStrLn $ "written (" ++ (show l) ++ " bytes)."

-- decompressionParameters = Z.defaultDecompressParams
decompressionParameters = Z.DecompressParams{..}
  where 
  decompressWindowBits = Z.windowBits 15
  decompressBufferSize = 100 * 1024 * 1024  -- 100MB
  decompressDictionary = Nothing 
  decompressAllMembers = False 




main' = do
  urls & traverse_ mainWith
  where
  urls = [ "http://httpbin.org/get", http://httpbin.org/deflate" ] 

mainWith url = do
  putStrLn $ "requesting " ++ url
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request manager

  let s = response & HTTP.responseStatus & HTTP.statusCode 
  let b = response & HTTP.responseBody

  putStrLn    $ "status code: " ++ show s
  B8.putStrLn $ "body:        " <> b 




main' = do
  urls & traverse_ mainWith
  where
  urls = 
    [ Nothing                 -: "http://httpbin.org/get"
    , Just (DEFLATE.decompress, "DEFLATE") -: "http://httpbin.org/deflate" 
    , Just    (GZIP.decompress, "GZIP")    -: "http://httpbin.org/deflate" 
    , Just    (ZLIB.decompress, "ZLIB")    -: "http://httpbin.org/deflate" 
    , Just (DEFLATE.decompress, "DEFLATE") -: "http://httpbin.org/gzip" 
    , Just    (GZIP.decompress, "GZIP")    -: "http://httpbin.org/gzip" 
    , Just    (ZLIB.decompress, "ZLIB")    -: "http://httpbin.org/gzip" 
    ]

{-

httpLbs :: Request -> Manager -> IO (Response ByteString)

decompressionParameters = Z.defaultDecompressParams

[http://httpbin.org/deflate]
"x\156=\141K\SO\194\&0\fD\247=E\228\&5D|%`\135P\ENQ\a\224\STX%1\137\165\&6\142\DC2\179\161\234\221IZ\137\229\188g\207\140\141R`\241\221w\130\SYN.J\210\aW\170B\143\157\197\148\v\ESCK,\224j\fFY\183\193\176\165\224\138\NUL\247\165\b\243y\241\&7\SO\SOH\141\DLE\135\170L\207\EM\255\238\193Y*\245\"\241EAsrP\204\180L\r(\158\235:\220\219\231\242\ETX\156\200\209\220\180;\232\163>m\244\246\188\135fj~s\144/\176"
NO

-}

mainWith (shouldDecompress,url) =  handle printingSomeException $ do
  putStrLn ""
  putStrLn "------------------------------------------"
  pause 50
  putStrLn $ "[requesting " ++ url ++ " ...]"
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request manager

  let s = response & HTTP.responseStatus & HTTP.statusCode 
  let b = response & HTTP.responseBody

  putStrLn    $ "status code:  " ++ show s
  putStrLn ""
  B8.putStrLn $ "body:         \n" <> b 
-- B.writeFile "data/zipped/httpbin_deflate.zip" b

  let doDecompress (_decompress,_method) = do
          putStrLn ""
          putStrLn $ "[decompressing with "<> _method <>"...]"
          let d = _decompress b
          let l = B.length d
          B8.putStrLn $ "decompressed: \n" <> d
          putStrLn    $ "length:       " <> show l 

  -- shouldDecompress & maybe nothing doDecompress 
  doDecompress `traverse_` shouldDecompress




-}

--------------------------------------------------------------------------------

main' = do
  files & traverse_ mainWith'
--  urls  & traverse_ mainWith

  where
  files =
    [ (unzipArchive, "ZIP") -: "data/zipped/AllSets-x.json.zip" 
    ]
  -- urls = 
  --   [ Nothing                    -: "http://httpbin.org/get"
  --   , Just (unzipArchive, "ZIP") -: "http://httpbin.org/deflate" 
  --   , Just (unzipArchive, "ZIP") -: "http://httpbin.org/gzip" 
  --   ]



mainWith' ((_decompress,_method),file) =  handle printingSomeException $ do
  putStrLn ""
  putStrLn "------------------------------------------"
  putStrLn $ "[reading " ++ file ++ " ...]"
  b <- B.readFile file 
    
  putStrLn ""
  putStrLn $ "[decompressing with "<> _method <>"...]"
  let d = _decompress b
  let l = B.length d
  B8.putStrLn $ "decompressed: \n" <> d
  putStrLn    $ "length:       " <> show l 

mainWith (shouldDecompress,url) =  handle printingSomeException $ do
  putStrLn ""
  putStrLn "------------------------------------------"
  pause 50
  putStrLn $ "[requesting " ++ url ++ " ...]"
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request manager

  let s = response & HTTP.responseStatus & HTTP.statusCode 
  let b = response & HTTP.responseBody

  putStrLn    $ "status code:  " ++ show s
  putStrLn ""
  B8.putStrLn $ "body:         \n" <> b 
-- B.writeFile "data/zipped/httpbin_deflate.zip" b

  let doDecompress (_decompress,_method) = do
          putStrLn ""
          putStrLn $ "[decompressing with "<> _method <>"...]"
          let d = _decompress b
          let l = B.length d
          B8.putStrLn $ "decompressed: \n" <> d
          putStrLn    $ "length:       " <> show l 

  -- shouldDecompress & maybe nothing doDecompress 
  doDecompress `traverse_` shouldDecompress

unzipArchive :: ByteString -> ByteString
unzipArchive = ZIP.toArchive >>> ZIP.zEntries >>> fmap ZIP.fromEntry >>> B.concat

