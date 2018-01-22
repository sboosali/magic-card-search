{-# LANGUAGE DataKinds, OverloadedLists, OverloadedStrings, TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-} 

module Card.JSONSchema where

-- import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString) 

import Data.Vinyl
import Data.Schematic

----------------------------------------

{-

@
Valid ReprObject {FieldRepr ReprArray [ReprNumber 13.0], FieldRepr ReprOptional Just ReprText "bar"}

ValidationError (MonoidMap (fromList [(".foo[0]",[".foo[0] should be > 10"])]))

ReprObject {FieldRepr ReprArray [ReprNumber 12.0], FieldRepr ReprOptional Just ReprText "bar"}

ReprObject {FieldRepr ReprArray [ReprNumber 12.0], FieldRepr ReprOptional Just ReprText "bar"}
@

-}
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

----------------------------------------

type SchemaExample
  = SchemaObject
    '[ '("foo", SchemaArray '[AEq 1] (SchemaNumber '[NGt 10]))
     , '("bar", SchemaOptional (SchemaText '[TEnum '["foo", "bar"]]))]

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