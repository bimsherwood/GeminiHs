module Handler (Handler(..), Handles, Handle) where

import Gemini (Path)
import Network.Simple.TCP.TLS (SockAddr)
import Network.TLS (Context)
  
type Handles = Path -> SockAddr -> IO Bool
type Handle = Path -> SockAddr -> Context -> IO ()

data Handler = Handler Handles Handle
