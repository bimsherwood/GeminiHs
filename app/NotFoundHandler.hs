module NotFoundHandler (createNotFoundHandler) where

import Gemini (Request(..), respondPermFailure, resolveVirtualPath)
import Handler (Handler(..), Handles, Handle)

alwaysHandles :: Handles
alwaysHandles _ = return True

handleNotFound :: (String -> IO ()) -> Handle
handleNotFound log (Request _ uri _) = do
  log $ "Resource not found: " ++ show uri
  return $ respondPermFailure "Resource not found."

createNotFoundHandler :: (String -> IO ()) -> Handler
createNotFoundHandler log = Handler alwaysHandles (handleNotFound log)
