module HsLib.Json where


import Data.Text
    
import HsLib.Json.Types (Json)

    
encode :: Json a => a -> Text
encode = undefined

encodeString :: Json a => a -> String
encodeString = undefined

decode :: Json a => Text -> Either String a               
decode = undefined

decodeString :: Json a => String -> Either String a
decodeString = undefined
