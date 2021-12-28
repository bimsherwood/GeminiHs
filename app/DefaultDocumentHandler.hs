module DefaultDocumentHandler (createDefaultDocumentHandler) where

import Prelude hiding (readFile)

import Config (SiteConfig(..))
import Data.ByteString.Lazy (readFile)
import Gemini (Path, Request(..), respondSuccess, resolveVirtualPath)
import Handler (Handler(..), Handles, Handle, nullHandler)
import System.Directory (doesFileExist)

defaultDocumentPath :: SiteConfig -> Path -> FilePath
defaultDocumentPath config requestPath =
  let siteRoot = cfgSiteRoot config
      dirPath = resolveVirtualPath siteRoot requestPath
      Just defaultDocName = cfgDefaultDocument config
      addSlash = last dirPath /= '/'
  in if addSlash 
    then dirPath ++ "/" ++ defaultDocName
    else dirPath ++ defaultDocName

handles :: SiteConfig -> Handles
handles config (Request _ _ path) =
  doesFileExist $ defaultDocumentPath config path

handle :: SiteConfig -> (String -> IO ()) -> Handle
handle config log (Request _ _ path) = do
  let filePath = defaultDocumentPath config path
  log ("Serving file " ++ filePath)
  fileContent <- readFile filePath
  return . respondSuccess "text/gemini" $ fileContent

defaultDocHandler :: SiteConfig -> (String -> IO ()) -> Handler
defaultDocHandler config log = Handler (handles config) (handle config log)

createDefaultDocumentHandler :: SiteConfig -> (String -> IO ()) -> Handler
createDefaultDocumentHandler config log =
  case cfgDefaultDocument config of
    Just defaultDocName -> defaultDocHandler config log
    Nothing             -> nullHandler