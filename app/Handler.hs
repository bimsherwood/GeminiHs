module Handler (Handler(..), Handles, Handle) where

import Gemini (Request)
import Network.Simple.TCP.TLS (SockAddr)
import Network.TLS (Context)
  
type Handles = Request -> SockAddr -> IO Bool
type Handle = Request -> SockAddr -> Context -> IO ()

data Handler = Handler Handles Handle
