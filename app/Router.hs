module Router (route) where

import Handler (Handler(..), Handle)

route :: [Handler] -> Handle
route [] _ _ _ = return ()
route ((Handler handles handle):xs) path addr ctxt = do
  applicable <- handles path addr
  if applicable
    then handle path addr ctxt
    else route xs path addr ctxt
