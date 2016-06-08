{-# LANGUAGE FlexibleInstances #-}
module HsLib.Json.Types (
                         JsonValue(..),
                         Json(..),
                        ) where

import Data.Text as T
import Data.Text.Encoding as TE
import Data.ByteString as BS
import Data.Map as M

data JsonValue = JsonBool !Bool
               | JsonNull
               | JsonObject !(Map Text JsonValue)
               | JsonArray ![JsonValue]
               --  Octal and hex forms are not allowed.  Leading zeros are not allowed. RFC4627-2.4
               | JsonNum !Double
               | JsonStr !Text
                 deriving (Show)



class Json a where
    toJson :: a -> JsonValue
    fromJson :: JsonValue -> Either String a

-- string-like
instance Json Char where
    toJson = JsonStr . T.pack . (:[])
    fromJson (JsonStr a) = case T.unpack a of
                             [c] -> Right $ c
                             _ -> Left "can't parse from value(exist more than 1 char in value) to Char"
    fromJson _ = Left "can't parse from value(not JsonStr) to Char"
                                 
    
instance Json String where
    toJson = JsonStr . T.pack
    fromJson (JsonStr a) = Right $ T.unpack a
    fromJson _ = Left "can't parse from value(not JsonStr) to String"
               
instance Json T.Text where
    toJson = JsonStr
    fromJson (JsonStr a) = Right a
    fromJson _ = Left "can't parse from value(not JsonStr) to Text"
               
instance Json BS.ByteString where
    toJson = JsonStr . TE.decodeUtf8
    fromJson (JsonStr a) = Right $ TE.encodeUtf8 a
    fromJson _ = Left "can't parse from value(not JsonStr) to ByteString"
                 
-- TODO:  BSL.ByteString 测试不实现lazy版本，会不会自动转化lazy到strict

-- Bool
instance Json Bool where
    toJson = JsonBool 
    fromJson (JsonBool a) = Right a
    fromJson _ = Left "can't parse from value(not JsonBool) to Bool"
                 
-- TODO: num

-- list-like 
instance Json a => Json [a] where
    toJson a = JsonArray $ toJson <$> a
    fromJson (JsonArray a) = mapM fromJson a
    fromJson _ = Left "can't parse from value(not JsonArray) to [a]"

-- TODO: (,) ...
                 
