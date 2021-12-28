module Handler (
  Handler(..),
  Handles,
  Handle,
  route) where

import Gemini (Request, Response)
  
type Handles = Request -> IO Bool
type Handle = Request -> IO Response

data Handler = Handler Handles Handle

route :: Handle -> [Handler] -> Handle
route defaultHandler [] request = defaultHandler request
route defaultHandler ((Handler handles handle):xs) request = do
  applicableHandler <- handles request
  if applicableHandler
    then handle request
    else route defaultHandler xs request
