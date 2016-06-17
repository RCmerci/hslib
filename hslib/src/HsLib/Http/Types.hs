{-# LANGUAGE OverloadedStrings #-}
module HsLib.Http.Types(
                        HttpVersion
                       ,httpVersion
                       ,Header(..)
                       ,GeneralHeader(..)
                       ,RequestHeader(..)
                       ,ResponseHeader(..)
                       ,EntityHeader(..)
                       ,Method(..)
                       ,StatusCode(..)
                       ,code2reason
                       ,RequestURI(..)
                       ,Request(..)
                       ,Response(..)
                       ) where

import Network.URI    

    
type HttpVersion = String
httpVersion = "1.1"
    
data Header = GeneralHeader GeneralHeader
            | RequestHeader RequestHeader
            | ResponseHeader ResponseHeader
            | EntityHeader EntityHeader
              deriving (Show)

                       
data GeneralHeader = CacheControl
                   | Connection
                   | Date
                   | Prama
                   | Trailer
                   | TransferEncoding
                   | Upgrade
                   | Via
                   | Warning
                     deriving (Show)

data RequestHeader = Accept
                   | AcceptCharset
                   | AcceptEncoding
                   | AcceptLanguage
                   | Authorization
                   | Expect
                   | From
                   | Host
                   | IfMatch
                   | IfModifiedSince
                   | IfNoneMatch
                   | IfRange
                   | IfUnmodifiedSince
                   | MaxForwards
                   | ProxyAuthorization
                   | Range
                   | Referer
                   | TE
                   | UserAgent
                     deriving (Show)
                              
data ResponseHeader = AcceptRanges
                    | Age
                    | ETag
                    | Location
                    | ProxyAuthenticate
                    | RetryAfter
                    | Server
                    | Vary
                    | WWWAuthenticate
                      deriving (Show)

data EntityHeader = Allow
                  | ContentEncoding
                  | ContentLanguage
                  | ContentLength
                  | ContentLocation
                  | ContentMD5
                  | ContentRange
                  | ContentType
                  | Expires
                  | LastModified
                  | ExtensionEntityHeader String
                    deriving (Show)
    
data Method = Options
            | Get
            | Head
            | Post
            | Put
            | Delete
            | Trace
            | Connect
            | ExtensionMethod String
              deriving (Show)

                       
newtype StatusCode = StatusCode (Int, Int, Int)

code2reason :: StatusCode -> String
code2reason (StatusCode (1,_,_)) = "informational"
code2reason (StatusCode (2,_,_)) = "success"
code2reason (StatusCode (3,_,_)) = "redirection"
code2reason (StatusCode (4,_,_)) = "client error"
code2reason (StatusCode (5,_,_)) = "server error"
code2reason (StatusCode _) = ""
    
--  http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
data RequestURI = Asterisk
                | AbsoluteURI URI
                | AbsPath String
                | Authority String
                  deriving (Show)
                           
data Request a = Request
    {
      method :: Method
    , uri :: RequestURI
    , requestHeaders :: [Header]
    , requestBody :: a
    }
              
data Response a = Response
    {
      statusCode :: StatusCode
    , reason :: String
    , responseHeaders :: [Header]
    , responseBody :: a
    }
