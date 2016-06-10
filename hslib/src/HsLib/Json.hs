module HsLib.Json (
                   Json(..)
                  , encode
                  , encodeString
                  , decode
                  , decodeString
                  ) where


import Data.Text
    
import HsLib.Json.Types (Json(..), encodeJsonValue)
import HsLib.Json.Parser (jsonParse)
import Text.Parsec.Error (ParseError)

    
encode :: Json a => a -> Text
encode = encodeJsonValue . toJson
    
    
encodeString :: Json a => a -> String
encodeString = unpack . encode

decode :: Json a => Text -> Either ParseError (Either String a)               
decode s = fromJson <$> (jsonParse s)

decodeString :: Json a => String -> Either ParseError (Either String a)
decodeString = decode . pack
