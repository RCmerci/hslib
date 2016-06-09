{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module HsLib.Json.Types (
                         JsonValue(..),
                         Json(..),
                        ) where

import Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as TLE
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Map as M

    
data JsonValue = JsonBool !Bool
               | JsonNull
               | JsonObject !(Map T.Text JsonValue)
               | JsonArray ![JsonValue]
               --  Octal and hex forms are not allowed.  Leading zeros are not allowed. RFC4627-2.4
               | JsonNum !Rational
               | JsonStr !T.Text
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
                                 
    
instance {-# OVERLAPPING #-} Json String where
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

instance Json BSL.ByteString where
    toJson = JsonStr . TL.toStrict . TLE.decodeUtf8
    fromJson (JsonStr a) = Right $ TLE.encodeUtf8 $ TL.fromStrict a


-- Bool
instance Json Bool where
    toJson = JsonBool 
    fromJson (JsonBool a) = Right a
    fromJson _ = Left "can't parse from value(not JsonBool) to Bool"
                 
-- Num
instance Json Word where
    toJson = JsonNum . fromIntegral
    fromJson (JsonNum a) = Right $ truncate a
    fromJson _ = Left "can't parse from value(not JsonNum) to Word"

instance Json Integer where
    toJson = JsonNum . fromIntegral
    fromJson (JsonNum a) = Right $ truncate a
    fromJson _ = Left "can't parse from value(not JsonNum) to Integer"

instance Json Int where
    toJson = JsonNum . fromIntegral
    fromJson (JsonNum a) = Right $ truncate a
    fromJson _ = Left "can't parse from value(not JsonNum) to Int"

instance Json Float where
    toJson = JsonNum . realToFrac
    fromJson (JsonNum a) = Right $ realToFrac a
    fromJson _ = Left "can't parse from value(not JsonNum) to Float"

instance Json Double where
    toJson = JsonNum . realToFrac
    fromJson (JsonNum a) = Right $ realToFrac a
    fromJson _ = Left "can't parse from value(not JsonNum) to Double"

instance Json Rational where
    toJson = JsonNum
    fromJson (JsonNum a) = Right a
    fromJson _ = Left "can't parse from value(not JsonNum) to Rational"

                 
-- list-like 
instance Json a => Json [a] where
    toJson a = JsonArray $ toJson <$> a
    fromJson (JsonArray a) = mapM fromJson a
    fromJson _ = Left "can't parse from value(not JsonArray) to [a]"

-- maybe
instance Json a => Json (Maybe a) where
    toJson (Just a) = JsonObject $ M.fromList $ [("Just", toJson a)]
    toJson Nothing = JsonObject $ M.fromList $ [("Nothing", JsonNull)]
    fromJson (JsonObject a) = case M.lookup "Just" a of
                                Just a' ->  Just <$> fromJson a'
                                Nothing -> case M.lookup "Nothing" a of
                                            Just JsonNull -> Right Nothing
                                            _ -> Left "can't parse from value to Maybe a"
    fromJson _ = Left "can't parse from value(not JsonObject) to Maybe a"


-- either
instance (Json a, Json b) => Json (Either a b) where
    toJson (Left a) = JsonObject $ M.fromList $ [("Left", toJson a)]
    toJson (Right b) = JsonObject $ M.fromList $ [("Right", toJson b)]
    fromJson (JsonObject a) = case M.lookup "Left" a of
                                Just a' -> Left <$> fromJson a'
                                Nothing -> case M.lookup "Right" a of
                                            Just a'' -> Right <$> fromJson a''
                                            _ -> Left "can't parse from value to Either a b"
    fromJson _ = Left "can't parse from value(not JsonObject) to Either a b"
                 
-- (,)
instance Json () where
    toJson () = JsonArray []
    fromJson (JsonArray []) = Right ()
    fromJson _ = Left "can't parse from value(exist more than 1 elem in JsonArray) to ()"

instance (Json a, Json b) => Json (a, b) where
    toJson (a, b) = JsonArray [toJson a, toJson b]
    fromJson (JsonArray [a, b]) = (,) <$> fromJson a <*> fromJson b
    fromJson _ = Left "can't parse from value(not exist 2 elems in JsonArray) to (a, b)"

instance (Json a, Json b, Json c) => Json (a, b, c) where
    toJson (a, b, c) = JsonArray [toJson a, toJson b, toJson c]
    fromJson (JsonArray [a, b, c]) = (,,) <$> fromJson a <*> fromJson b <*> fromJson c


---- TODO:  map
-- instance Json a => Json (M.Map T.Text a) where
--     toJson = 
