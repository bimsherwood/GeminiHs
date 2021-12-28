module Handler (
  Handler(..),
  Handles,
  Handle,
  nullHandler,
  Router,
  route) where

import Gemini (Request, respondPermFailure, Response)
  
type Handles = Request -> IO Bool
type Handle = Request -> IO Response
type Router = Request -> IO Response

data Handler = Handler Handles Handle

nullHandler :: Handler
nullHandler = Handler
  (\_ -> return False)
  (\_ -> return . respondPermFailure $ "Null request handler")

route :: Handle -> [Handler] -> Handle
route defaultHandler [] request = defaultHandler request
route defaultHandler ((Handler handles handle):xs) request = do
  applicableHandler <- handles request
  if applicableHandler
    then handle request
    else route defaultHandler xs request
