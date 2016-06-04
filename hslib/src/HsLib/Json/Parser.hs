{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module HsLib.Json.Parser ( value
                         , Value(..)
                         , JsonP(..)
                         ) where

    
import Control.Monad
import Data.Text hiding (concat)
import Data.Functor.Identity
import Data.Map.Strict (Map, fromList)
import Text.Parsec as P
import Text.Parsec.Prim
    
type JsonParser t a = forall s m . Stream s m t => ParsecT s () m a
type JsonP a = JsonParser Char a

data Value = JsonBool !Bool
           | JsonNull
           | JsonObject !(Map Text Value)
           | JsonArray ![Value]
           --  Octal and hex forms are not allowed.  Leading zeros are not allowed. RFC4627-2.4
           | JsonNum !Double
           | JsonStr !Text
             deriving (Show)
    
colon :: JsonP Char
colon = char ':'
        
comma :: JsonP Char
comma = char ','
        
ws :: JsonP ()
ws = skipMany space

withWsAround :: JsonP a -> JsonP ()
withWsAround p = ws >> p >> ws
     
beginArray :: JsonP ()
beginArray = try (withWsAround (char '[')) <?> "beginArray:\"[\""

beginObject :: JsonP ()
beginObject = try (withWsAround (char '{')) <?> "beginObject:\"{\""

endArray :: JsonP ()
endArray = try (withWsAround (char ']')) <?> "endArray:\"]\""

endObject :: JsonP ()
endObject = try (withWsAround (char '}')) <?> "endObject:\"}\""

nameSeparator :: JsonP ()
nameSeparator = try (withWsAround colon) <?> "nameSeparator:\":\""

valueSeparator :: JsonP ()
valueSeparator = try (withWsAround comma) <?> "valueSeparator:\",\""

jsonString :: JsonP String
jsonString = between (char '"') (char '"') $
             join <$> many (liftM return normal <|> escape)
    where normal = noneOf "\\\""
          escape = do
            char '\\' 
            a <- choice $ (:) (char 'u' >> P.count 4 hexDigit >>= return . ((++) "u"))
                (fmap P.string ["\"", "\\", "/", "b", "f", "n", "r", "t"])
            return $ "\\" ++ a
                   
              
num :: JsonP Double
num = (\a b c -> read $ concat [a,b,c]) <$> beforeDot <*> afterDotbeforeE <*> afterE
      where beforeDot = do {char '-';
                            n <- many digit;
                            return $ "-" ++ n}
                        <|> many1 digit
            afterDotbeforeE = ((char '.' >> many digit) <|> return "0") >>= \s -> return ("." ++ s)
            afterE = (((char 'e' <|> char 'E') >> do
                         v1 <- char '+' <|> char '-' <|> digit
                         v2 <- many digit
                         return $ v1 : v2)
                     <|> return "0") >>= \s -> return ("E" ++ s)
             
false :: JsonP Value
false = P.string "false" >> return (JsonBool False)

true :: JsonP Value
true = P.string "true" >> return (JsonBool True)

null :: JsonP Value
null = P.string "null" >> return JsonNull

object :: JsonP Value
object = between beginObject endObject $ do
           kv <- member `sepBy` try valueSeparator
           return $ JsonObject $ fromList kv
         where member = do
                 k <- jsonString
                 nameSeparator
                 v <- value
                 return $ (pack k, v)

array :: JsonP Value
array = between beginArray endArray $ JsonArray <$> value `sepBy` valueSeparator

number :: JsonP Value
number = JsonNum <$> num

string :: JsonP Value
string = JsonStr . pack <$> jsonString

value :: JsonP Value
value = ws >> choice [false,    -- f
                      true,     -- t
                      HsLib.Json.Parser.null,     -- n
                      object,   -- {
                      array,    -- [
                      number,   -- -|digit
                      HsLib.Json.Parser.string]   -- "


