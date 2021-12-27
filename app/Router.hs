module Router (route) where

import Handler (Handler(..), Handle)

route :: [Handler] -> Handle
route [] _ _ _ = return ()
route ((Handler handles handle):xs) request addr ctxt = do
  applicable <- handles request addr
  if applicable
    then handle request addr ctxt
    else route xs request addr ctxt
