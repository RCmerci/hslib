module HsLib.Http.Types() where

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

                       
newtype StatausCode = StatausCode (Int, Int, Int)
