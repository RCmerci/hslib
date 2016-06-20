{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- TODO: explain this extension

module HsLib.Http.Base () where

import Network.Socket
import Control.Monad.Reader

    
import HsLib.Http.Types

type Connection = (Socket, SockAddr)

newtype ConnectionCtx a = ConnectionCtx (Reader Connection a)
    deriving (Functor, Applicative, Monad, MonadReader Connection)
    
    
request :: Request a -> ConnectionCtx (Response a)
request = undefined


          
