{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module HsLib.Http.Types(
                        HttpVersion
                       ,httpVersion
                       -- ,Header(..)
                       ,RqHeader(..)
                       ,RpHeader(..)
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
import Data.Char (toUpper)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
    
type HttpVersion = String
httpVersion = "HTTP/1.1"
    

data RqHeader = RqGeneralHeader GeneralHeader
               | RqRequestHeader RequestHeader
               | RqEntityHeader EntityHeader
                 
data RpHeader = RpGeneralHeader GeneralHeader
              | RpResponseHeader ResponseHeader
              | RpEntityHeader EntityHeader
                       
data GeneralHeader = CacheControl String
                   | Connection String
                   | Date String
                   | Prama String
                   | Trailer String
                   | TransferEncoding String
                   | Upgrade String
                   | Via String
                   | Warning String

data RequestHeader = Accept String
                   | AcceptCharset String
                   | AcceptEncoding String
                   | AcceptLanguage String
                   | Authorization String
                   | Expect String
                   | From String
                   | Host String
                   | IfMatch String
                   | IfModifiedSince String
                   | IfNoneMatch String
                   | IfRange String
                   | IfUnmodifiedSince String
                   | MaxForwards String
                   | ProxyAuthorization String
                   | Range String
                   | Referer String
                   | TE String
                   | UserAgent String
                              
data ResponseHeader = AcceptRanges String
                    | Age String
                    | ETag String
                    | Location String
                    | ProxyAuthenticate String
                    | RetryAfter String
                    | Server String
                    | Vary String
                    | WWWAuthenticate String

data EntityHeader = Allow String
                  | ContentEncoding String
                  | ContentLanguage String
                  | ContentLength String
                  | ContentLocation String
                  | ContentMD5 String
                  | ContentRange String
                  | ContentType String
                  | Expires String
                  | LastModified String
                  | ExtensionEntityHeader String String
    
data Method = Options
            | Get
            | Head
            | Post
            | Put
            | Delete
            | Trace
            | Connect
            | ExtensionMethod String

                       
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
                           
data Request a = Request
    {
      method :: Method
    , uri :: RequestURI
    , requestHeaders :: [RqHeader]
    , requestBody :: a
    }
              
data Response a = Response
    {
      statusCode :: StatusCode
    , reason :: String
    , responseHeaders :: [RpHeader]
    , responseBody :: a
    }



instance Show Method where
    show Options = "OPTIONS"
    show Get = "GET"
    show Head = "HEAD"
    show Post = "POST"
    show Put = "PUT"
    show Delete = "DELETE"
    show Trace = "TRACE"
    show Connect = "CONNECT"
    show (ExtensionMethod s) = fmap toUpper s

instance Show GeneralHeader where
    show (CacheControl s) = "Cache-Control: " ++ s
    show (Connection s) = "Connection: " ++ s
    show (Date s) = "Date: " ++ s
    show (Prama s) = "Prama: " ++ s
    show (Trailer s) = "Trailer: " ++ s
    show (TransferEncoding s) = "Transfer-Encoding: " ++ s
    show (Upgrade s) = "Upgrade: " ++ s
    show (Via s) = "Via: " ++ s
    show (Warning s) = "Warning: " ++ s

instance Show RequestHeader where
    show (Accept s) = "Accept: " ++ s
    show (AcceptCharset s) = "Accept-Charset: " ++ s
    show (AcceptEncoding s) = "Accept-Encoding: " ++ s
    show (AcceptLanguage s) = "Accept-Language: " ++ s
    show (Authorization s) = "Authorization: " ++ s
    show (Expect s) = "Expect: " ++ s
    show (From s) = "From: " ++ s
    show (Host s) = "Host: " ++ s
    show (IfMatch s) = "If-Match: " ++ s
    show (IfModifiedSince s) = "If-Modified-Since: " ++ s
    show (IfNoneMatch s) = "If-None-Match: " ++ s
    show (IfRange s) = "If-Range: " ++ s
    show (IfUnmodifiedSince s) = "If-Unmodified-Since: " ++ s
    show (MaxForwards s) = "Max-Forwards: " ++ s
    show (ProxyAuthorization s) = "Proxy-Authorization: " ++ s
    show (Range s) = "Range: " ++ s
    show (Referer s) = "Referer: " ++ s
    show (TE s) = "TE: " ++ s
    show (UserAgent s) = "User-Agent: " ++ s

instance Show ResponseHeader where
    show (AcceptRanges s) = "Accept-Ranges: " ++ s
    show (Age s) = "Age: " ++ s
    show (ETag s) = "ETag: " ++ s
    show (Location s) = "Location: " ++ s
    show (ProxyAuthenticate s) = "Proxy-Authenticate: " ++ s
    show (RetryAfter s) = "Retry-After: " ++ s
    show (Server s) = "Server: " ++ s
    show (Vary s) = "Vary: " ++ s
    show (WWWAuthenticate s) = "WWW-Authenticate: " ++ s

instance Show EntityHeader where
    show (Allow s) = "Allow: " ++ s
    show (ContentEncoding s) = "ContentEncoding: " ++ s
    show (ContentLanguage s) = "ContentLanguage: " ++ s
    show (ContentLength s) = "ContentLength: " ++ s
    show (ContentLocation s) = "ContentLocation: " ++ s
    show (ContentMD5 s) = "ContentMD5: " ++ s
    show (ContentRange s) = "ContentRange: " ++ s
    show (ContentType s) = "ContentType: " ++ s
    show (Expires s) = "Expires: " ++ s
    show (LastModified s) = "LastModified: " ++ s
    show (ExtensionEntityHeader headername s) = headername ++ ": " ++ s

instance Show RqHeader where
    show (RqGeneralHeader h) = show h
    show (RqRequestHeader h) = show h
    show (RqEntityHeader h) = show h
                            
instance Show RpHeader where
    show (RpGeneralHeader h) = show h
    show (RpResponseHeader h) = show h
    show (RpEntityHeader h) = show h

instance Show RequestURI where
    show Asterisk = "*"
    show (AbsoluteURI uri) = show uri
    show (AbsPath s) = s
    show (Authority s) = s

instance Show StatusCode where
    show (StatusCode (a,b,c)) = concat $ fmap show [a,b,c]


instance {-# OVERLAPPING #-}Show (Request String) where
    show Request{..} =
        show method ++ sp ++ show uri ++ sp ++ httpVersion ++ crlf ++
        concat (fmap ((++crlf) . show) requestHeaders) ++ crlf ++
        requestBody

instance {-# OVERLAPPING #-}Show (Request T.Text) where
    show r@Request{..} = show r{requestBody=T.unpack requestBody}
                         
instance {-# OVERLAPPING #-}Show (Request TL.Text) where
    show r@Request{..} = show r{requestBody=TL.unpack requestBody}
                         
instance {-# OVERLAPPING #-}Show (Request B.ByteString) where
    show r@Request{..} = show r{requestBody=E.decodeUtf8 requestBody}

instance {-# OVERLAPPING #-}Show (Request BL.ByteString) where
    show r@Request{..} = show r{requestBody=LE.decodeUtf8 requestBody}
                         
instance Show a => Show (Request a) where
    show r@Request{..} = show r{requestBody=show requestBody}

 
instance {-# OVERLAPPING #-}Show (Response String) where
    show Response{..} =
        httpVersion ++ sp ++ show statusCode ++ sp ++ reason ++ crlf ++ 
        concat (fmap ((++crlf) . show) responseHeaders) ++ crlf ++
        responseBody
        
instance {-# OVERLAPPING #-}Show (Response T.Text) where
    show r@Response{..} = show r{responseBody=T.unpack responseBody}
                          
instance {-# OVERLAPPING #-}Show (Response TL.Text) where
    show r@Response{..} = show r{responseBody=TL.unpack responseBody}
                          
instance {-# OVERLAPPING #-}Show (Response B.ByteString) where
    show r@Response{..} = show r{responseBody=E.decodeUtf8 responseBody}

instance {-# OVERLAPPING #-}Show (Response BL.ByteString) where
    show r@Response{..} = show r{responseBody=LE.decodeUtf8 responseBody}
                          
instance Show a => Show (Response a) where
    show r@Response{..} = show r{responseBody=show responseBody}

             
crlf = "\r\n"                       
sp = " "
