module Router (route) where

import Gemini (respondPermFailure)
import Handler (Handler(..), Handle)

noHandlerError :: String
noHandlerError = "No appropriate handler was found for this request."

route :: [Handler] -> Handle
route [] _ = return . respondPermFailure $ noHandlerError
route ((Handler handles handle):xs) request = do
  applicableHandler <- handles request
  if applicableHandler
    then handle request
    else route xs request
