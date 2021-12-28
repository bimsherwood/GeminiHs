module Handler (Handler(..), Handles, Handle) where

import Gemini (Request, Response)
import Network.TLS (Context)
  
type Handles = Request -> IO Bool
type Handle = Request -> IO Response

data Handler = Handler Handles Handle
